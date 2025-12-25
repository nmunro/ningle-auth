# ningle-auth

**Version:** 0.2.0
**Author:** nmunro
**License:** BSD3-Clause

A complete authentication system for [Ningle](https://github.com/fukamachi/ningle) web applications, providing user registration, email verification, login, logout, and password reset functionality.

## Features

- **User Registration** with email verification
- **Email Verification** with expiring tokens
- **Login/Logout** with session management
- **Password Reset** with secure token-based workflow
- **Role-Based Access Control** via permissions system
- **Extensible Token System** - register custom token purposes
- **CSRF Protection** on all forms
- **Comprehensive Test Suite** - 51 tests, 91 assertions

## Dependencies

- [ningle](https://github.com/fukamachi/ningle) - Web framework
- [clack](https://github.com/fukamachi/clack) - Web server interface
- [mito](https://github.com/fukamachi/mito) - ORM and database migrations
- [mito-auth](https://github.com/fukamachi/mito-auth) - Password hashing
- [cu-sith](https://github.com/nmunro/cu-sith) - Authentication framework
- [envy](https://github.com/fukamachi/envy) & [envy-ningle](https://github.com/nmunro/envy-ningle) - Configuration management
- [djula](https://github.com/mmontone/djula) - Templating engine
- [cl-forms](https://github.com/mmontone/cl-forms) - Form validation
- [ningle-email](https://github.com/nmunro/ningle-email) - Email delivery
- [ironclad](https://github.com/sharplispers/ironclad) - Cryptography
- [local-time](https://github.com/dlowe-net/local-time) - Time handling

## Installation

```lisp
;; Clone into Quicklisp local-projects
cd ~/quicklisp/local-projects/
git clone https://github.com/nmunro/ningle-auth.git

;; Load the system
(ql:quickload :ningle-auth)
```

## Configuration

### Required Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `ENVY_CONFIG_PACKAGE` | Package containing configuration | `"your-app/config"` |
| `APP_ENV` | Application environment | `"development"`, `"production"`, `"test"` |

Optional environment variables:
- `SQLITE_DB_NAME` - Database path (default: see your config)

### Configuration Options

Create a config package in your application:

```lisp
(defpackage your-app/config
  (:use :cl :envy))

(in-package your-app/config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  '(:auth-mount-path "/auth"        ; Base path for auth routes (required)
    :token-expiration 3600           ; Token lifetime in seconds (default: 1 hour)
    :login-redirect "/"              ; Redirect after successful login
    :email-backend :smtp             ; Email backend (:smtp, :sendmail, :string)
    :smtp-host "smtp.example.com"
    :smtp-port 587
    :smtp-username "user@example.com"
    :smtp-password "password"))

(defconfig |development|
  '(:debug T
    :middleware ((:session :state (:file :directory "/tmp"))
                 (:mito (:sqlite3 :database-name "dev.db")))))

(defconfig |production|
  '(:debug NIL
    :middleware ((:session)
                 (:mito (:postgres :database-name "myapp"
                                   :username "dbuser"
                                   :password "dbpass")))))
```

**Important:** The `:auth-mount-path` configuration is required and determines where authentication routes are mounted.

## Database Models

### User
Stores user account information.

**Columns:**
- `id` (INTEGER, primary key)
- `email` (VARCHAR 255, unique, not null)
- `username` (VARCHAR 255, unique, not null)
- `active` (INTEGER, default: 0) - 0=inactive, 1=active
- `password_hash` (CHAR 64, not null)
- `password_salt` (CHAR 64, not null)
- `created_at` (TIMESTAMP)
- `updated_at` (TIMESTAMP)

**Methods:**
- `(ningle-auth/models:activate user)` - Activate a user account

### Role
Defines user roles for access control.

**Columns:**
- `id` (INTEGER, primary key)
- `name` (VARCHAR 255, unique, not null)
- `description` (VARCHAR 2048, not null)
- `created_at` (TIMESTAMP)
- `updated_at` (TIMESTAMP)

### Permission
Links users to roles (many-to-many relationship).

**Columns:**
- `id` (INTEGER, primary key)
- `user_id` (INTEGER, foreign key → user.id)
- `role_id` (INTEGER, foreign key → role.id)
- `created_at` (TIMESTAMP)
- `updated_at` (TIMESTAMP)

**Constraints:**
- Unique combination of (user_id, role_id)

### Token
Stores time-limited tokens for email verification and password reset.

**Columns:**
- `id` (INTEGER, primary key)
- `user_id` (INTEGER, foreign key → user.id)
- `purpose` (TEXT, not null) - "email-verification" or "password-reset"
- `token` (VARCHAR 64, not null) - SHA-256 hash
- `salt` (BINARY, not null)
- `expires_at` (TIMESTAMP, not null)
- `created_at` (TIMESTAMP)
- `updated_at` (TIMESTAMP)

**Constraints:**
- Unique combination of (user_id, purpose)

**Methods:**
- `(ningle-auth/models:generate-token user purpose &key expires-in)` - Create a token
- `(ningle-auth/models:is-expired-p token)` - Check if token is expired

## Routes

All routes are mounted at the path specified by `:auth-mount-path` (default: `/auth`).

### GET/POST /register
User registration with email verification.

**GET:** Display registration form
**POST:** Create user account, send verification email

**Form Fields:**
- `email` (email, required, validated)
- `username` (string, required, min 1 char)
- `password` (password, required, min 8 chars)
- `password-verify` (password, required, must match password)

**Flow:**
1. User submits registration form
2. System creates inactive user (`active=0`)
3. System generates email verification token
4. Verification email sent with link to `/verify`
5. User redirected to homepage

**Errors:**
- Duplicate username or email
- Password mismatch
- Validation failures
- CSRF token mismatch (403)

### GET /verify
Email verification endpoint.

**Query Parameters:**
- `user` (string, required) - Username
- `token` (string, required) - 64-char hex token

**Flow:**
1. Validates token for user
2. If expired: generates new token, sends new email
3. If valid: activates user (`active=1`), creates "user" role permission, deletes token
4. Redirects to `/auth/login`

**Errors:**
- Invalid or missing token
- User already logged in (redirects to `/`)

### GET/POST /login
User authentication.

**GET:** Display login form
**POST:** Authenticate user and create session

**Form Fields:**
- `username` (string, required)
- `password` (password, required)

**Flow:**
1. User submits credentials
2. System verifies username and password
3. Checks user is active
4. Creates session via cu-sith
5. Redirects to configured `:login-redirect` path

**Errors:**
- Invalid username (suggests checking verification)
- Invalid password
- User already logged in (redirects to `/`)
- CSRF token mismatch (403)

### GET/POST /logout
End user session.

**Methods:** GET or POST
**Authentication:** Must be logged in

**Flow:**
1. Destroys user session
2. Redirects to configured `:login-redirect` path

### GET/POST /reset
Request password reset.

**GET:** Display password reset request form
**POST:** Generate reset token and send email

**Form Fields:**
- `email` (string, required)

**Flow:**
1. User submits email address
2. System finds user by email
3. If existing unexpired token: error message
4. If expired token: deletes old token, generates new one
5. Sends password reset email with link to `/reset/process`
6. Redirects to homepage

**Errors:**
- No user found with that email
- Reset already in progress
- User already logged in (redirects to `/`)
- CSRF token mismatch (403)

### GET/POST /reset/process
Complete password reset.

**Query Parameters (GET only):**
- `user` (string, required) - Username
- `token` (string, required) - 64-char hex token

**GET:** Display password reset form (if token valid)
**POST:** Update password and complete reset

**Form Fields (POST):**
- `email` (hidden, required)
- `token` (hidden, required)
- `password` (password, required, min 8 chars)
- `password-verify` (password, required, must match password)

**Flow:**
1. User clicks link from email (GET with user/token params)
2. System validates token
3. If valid: displays password reset form
4. User submits new password (POST)
5. System updates password hash, deletes token
6. Redirects to `/auth/login`

**Errors:**
- Invalid or expired token
- Password mismatch
- User already logged in (redirects to `/`)
- CSRF token mismatch (403)

## Forms

All forms include CSRF protection with a `csrftoken` field.

### Register Form
- Email (email field, valid email format required)
- Username (string, not blank)
- Password (password, min 8 characters)
- Password Verify (password, min 8 characters)
- Submit button

### Login Form
- Username (string)
- Password (password)
- Submit button

### Reset Password Form
- Email (string)
- Submit button

### New Password Form
- Email (hidden field, valid email format)
- Token (hidden field, exactly 64 characters)
- Password (password, min 8 characters)
- Password Verify (password, min 8 characters)
- Submit button

## Token Registry

**New in 0.2.0:** Extensible token purpose registration system.

### Built-in Token Purposes

- `"email-verification"` - Email verification during registration
- `"password-reset"` - Password reset tokens

### Registering Custom Token Purposes

```lisp
(use-package :ningle-auth/token-registry)

;; Register a new token purpose
(register-token-purpose "two-factor-auth")
(register-token-purpose "magic-link")
(register-token-purpose "account-deletion")

;; List all registered purposes
(list-token-purposes)
;; => ("email-verification" "password-reset" "two-factor-auth" "magic-link" "account-deletion")

;; Check if a purpose is valid
(token-purpose-valid-p "two-factor-auth")  ;; => T
(token-purpose-valid-p "invalid")          ;; => NIL
```

### Using Custom Tokens

```lisp
;; Generate a custom token
(let* ((user (mito:find-dao 'ningle-auth/models:user :username "alice"))
       (token (ningle-auth/models:generate-token user "two-factor-auth"
                                                  :expires-in 300))) ; 5 minutes
  ;; Send token via SMS, email, etc.
  (send-2fa-code user (ningle-auth/models:value token)))
```

### API Reference

**Package:** `ningle-auth/token-registry`

- `(register-token-purpose purpose)` - Register a new token purpose (string)
- `(token-purpose-valid-p purpose)` - Check if purpose is registered
- `(list-token-purposes)` - Return list of all registered purposes
- `+email-verification+` - Constant for built-in email verification purpose
- `+password-reset+` - Constant for built-in password reset purpose

## Testing

Run the test suite:

```lisp
(asdf:test-system :ningle-auth)
```

**Test Coverage:**
- 51 test functions
- 91 assertions
- Model tests (36 tests) - User, Role, Permission, Token
- Form tests (8 tests) - Validation for all forms
- Route tests (7 tests) - Route registration verification

**Test Features:**
- In-memory SQLite database for fast execution
- Isolated test environment with fixtures
- Email backend testing via `:string` backend
- Database setup/teardown hooks

## Integration Example

```lisp
(defpackage myapp
  (:use :cl))

(in-package :myapp)

;; Set up environment
(setf (uiop:getenv "ENVY_CONFIG_PACKAGE") "myapp/config")
(setf (uiop:getenv "APP_ENV") "development")

;; Create your main app
(defvar *main-app* (make-instance 'ningle:<app>))

;; Mount ningle-auth at /auth
(setf (ningle:route *main-app* "/auth*")
      ningle-auth:*app*)

;; Your application routes
(setf (ningle:route *main-app* "/")
      (lambda (params)
        (if (cu-sith:logged-in-p)
            (format nil "Welcome, ~A!" (cu-sith:username (cu-sith:get-user)))
            (format nil "Please <a href='/auth/login'>login</a> or <a href='/auth/register'>register</a>"))))

;; Protected route example
(setf (ningle:route *main-app* "/dashboard")
      (lambda (params)
        (if (cu-sith:logged-in-p)
            (format nil "Dashboard for ~A" (cu-sith:username (cu-sith:get-user)))
            (ingle:redirect "/auth/login"))))

;; Start the server
(defun start-server ()
  (ningle-auth/migrations:migrate)  ; Run migrations
  (clack:clackup
   (lack.builder:builder
    (envy-ningle:build-middleware :myapp/config *main-app*))
   :server :woo
   :port 8000))
```

## Database Migrations

Migrations are managed by Mito:

```lisp
;; Run migrations
(ningle-auth/migrations:migrate)

;; In your application startup
(defun initialize-db ()
  (mito:connect-toplevel :postgres :database-name "myapp"
                                   :username "dbuser"
                                   :password "dbpass")
  (ningle-auth/migrations:migrate))
```

## Exported Symbols

**Main Package:** `ningle-auth`
- `*app*` - The Ningle application instance
- `start` - Start the authentication server (standalone)
- `stop` - Stop the authentication server

**Models Package:** `ningle-auth/models`
- Model classes: `user`, `role`, `permission`, `token`
- Accessors: `id`, `email`, `username`, `active`, `name`, `description`, `purpose`, `salt`, `value`, `expires-at`, `created-at`, `updated-at`, `password-hash`
- Methods: `activate`, `generate-token`, `is-expired-p`
- Constants: `+email-verification+`, `+password-reset+`

**Token Registry Package:** `ningle-auth/token-registry`
- Functions: `register-token-purpose`, `token-purpose-valid-p`, `list-token-purposes`
- Constants: `+email-verification+`, `+password-reset+`

**Forms Package:** `ningle-auth/forms`
- Forms: `register`, `login`, `reset-password`, `new-password`
- Field names: `email`, `username`, `token`, `password`, `password-verify`

## Security Features

- **Password Hashing:** Uses mito-auth with secure hashing (SHA-256 with salt)
- **CSRF Protection:** All forms include CSRF tokens
- **Token Expiration:** Time-limited tokens for email verification and password reset
- **Session Management:** Secure session handling via cu-sith
- **Input Validation:** Form validation with clavier
- **Active User Check:** Only active users can log in
- **Token Uniqueness:** One active token per user per purpose

## Email Templates

Templates are stored in `src/templates/ningle-auth/email/`:
- `register.txt` - Email verification message
- `reset.txt` - Password reset message

Both templates support Djula templating with variables:
- `{{ user }}` - User object
- `{{ link }}` - Verification/reset link

## Troubleshooting

### "Missing ENVY configuration package" error
Ensure `ENVY_CONFIG_PACKAGE` environment variable is set before loading the system.

### "Route not found" errors
Check that `:auth-mount-path` is configured in your config package.

### Email not sending
- Verify email backend configuration (`:email-backend`, `:smtp-host`, etc.)
- Check ningle-email is properly configured
- Use `:string` backend for testing

### Users can't log in after registration
- Check email verification workflow - users must verify email before logging in
- Look for "have you verified the account?" message on login errors

### Token expired errors
- Check `:token-expiration` config (default 3600 seconds = 1 hour)
- For custom tokens, pass `:expires-in` parameter to `generate-token`

## Contributing

Issues and pull requests welcome at the project repository.

## License

BSD3-Clause

Copyright (c) 2024 nmunro

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the conditions of the BSD 3-Clause License are met.
