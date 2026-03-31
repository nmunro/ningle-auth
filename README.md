# ningle-auth

**Version:** 0.3.0
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
- [ingle](https://github.com/fukamachi/ingle) - Ningle utilities (redirects, param helpers)
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
- [cl-trivial-clock](https://github.com/ak-coram/cl-trivial-clock) - **Indirect dependency** (required by cl-smtp → frugal-uuid, explicit due to qlot limitation with GitHub packages)

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

ningle-auth reads configuration via `envy-ningle:get-config` at runtime. Your application must provide a config package using [envy](https://github.com/fukamachi/envy) and [envy-ningle](https://github.com/nmunro/envy-ningle). The following keys are required:

| Key | Type | Description |
|-----|------|-------------|
| `:auth-mount-path` | string | Path where auth routes are mounted (e.g. `"/auth"`) |
| `:login-redirect` | string | Where unauthenticated users are sent (e.g. `"/auth/login"`) |
| `:login-success-redirect` | string | Where to redirect after successful login (e.g. `"/"`) |
| `:token-expiration` | integer | Token lifetime in seconds (e.g. `3600` for 1 hour) |
| `:project-name` | string | Used in email subjects (e.g. `"My App"`) |
| `:email-backend` | keyword | Email backend — `:console`, `:smtp`, or `:sendmail` |
| `:email-default-from` | string | From address for outgoing emails |
| `:email-reply-to` | string | Reply-to address for outgoing emails |
| `:email-admins` | list | List of admin email addresses |

The `:invite-token-expiration` key is also expected if you register invite-style token purposes.

Example config package:

```lisp
(defpackage your-app/config
  (:use :cl :envy))

(in-package your-app/config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  '(:auth-mount-path "/auth"
    :login-redirect "/auth/login"        ; Where unauthenticated users are sent
    :login-success-redirect "/"          ; Where to go after successful login
    :token-expiration 3600
    :project-name "My App"
    :email-admins ("admin@example.com")))

(defconfig |development|
  '(:debug T
    :email-backend :console
    :email-default-from "noreply@example.com"
    :email-reply-to "noreply@example.com"))

(defconfig |production|
  '(:debug NIL
    :email-backend :smtp
    :email-default-from "noreply@example.com"
    :email-reply-to "noreply@example.com"))
```

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
5. Redirects to configured `:login-success-redirect` path

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

**New in 0.3.0:** Extensible token purpose registration system.

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

;; Register a prefix-based token purpose
(register-token-purpose "api-key-" :prefix t)

;; Check if a purpose is valid
(token-purpose-valid-p "two-factor-auth")  ;; => T (exact match)
(token-purpose-valid-p "api-key-read")     ;; => T (prefix match)
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

- `(register-token-purpose purpose &key prefix)` - Register a new token purpose (string). Use `:prefix t` for prefix-based matching.
- `(token-purpose-valid-p purpose)` - Check if purpose is registered (supports exact match and prefix matching)
- `+email-verification+` - Constant for built-in email verification purpose
- `+password-reset+` - Constant for built-in password reset purpose

## Testing

Run the test suite:

```lisp
(asdf:test-system :ningle-auth)
```

## Integration Example

```lisp
(defpackage myapp
  (:use :cl)
  (:import-from :ningle-auth :login-required))

(in-package :myapp)

;; Create your main app
(defvar *app* (make-instance 'ningle:<app>))

;; Initialise ningle-auth (call before defining routes)
(ningle-auth:setup)

;; Your application routes
(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (if (cu-sith:logged-in-p)
            (let ((user (cu-sith:user)))
              (format nil "Welcome, ~A!" (ningle-auth/models:username user)))
            "Please <a href='/auth/login'>login</a> or <a href='/auth/register'>register</a>")))

;; Protected route — redirects to :login-redirect if not authenticated
(setf (ningle:route *app* "/dashboard")
      (login-required
        (lambda (params)
          (declare (ignore params))
          (let ((user (cu-sith:user)))
            (format nil "Dashboard for ~A" (ningle-auth/models:username user))))))

;; Build the middleware stack — this must be the last form evaluated,
;; as envy-ningle:build-middleware returns the composed lack app that
;; clackup (or clack:clackup) will serve.
(envy-ningle:build-middleware :myapp/config *app*)
```

`envy-ningle:build-middleware` mounts `ningle-auth:*app*` at `:auth-mount-path` via lack's `:mount` middleware. The return value of the last form is what clackup uses as the application.

## Database Migrations

ningle-auth ships migration files alongside the library. Migrations are managed by [mito](https://github.com/fukamachi/mito) via your application's database connection — run them with your application's migration tooling after connecting to the database.

## Exported Symbols

**Main Package:** `ningle-auth`
- `*app*` - The Ningle application instance (mount this via `:mount` middleware)
- `setup` - Initialise ningle-auth; must be called before serving requests (see below)
- `login-required` - Middleware wrapper that redirects unauthenticated requests to `:login-redirect`

### `setup` keyword arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `:user` | `ningle-auth/models:user` | The user model class to use |
| `:user-p` | internal `get-user` | Function `(username) → user-or-nil` for looking up users |
| `:default-roles` | `("user")` | Role names automatically assigned on email verification |
| `:extra-token-purposes` | `nil` | Additional token purposes to register; list of `(purpose &key prefix)` lists |

Example with all options:

```lisp
(ningle-auth:setup
  :user 'myapp/models:user
  :default-roles '("member")
  :extra-token-purposes '(("invite:" :prefix t)
                          ("magic-link")))
```

**Models Package:** `ningle-auth/models`
- Model classes: `user`, `role`, `permission`, `token`
- Accessors: `id`, `email`, `username`, `active`, `name`, `description`, `purpose`, `salt`, `value`, `expires-at`, `created-at`, `updated-at`, `password-hash`
- Methods: `activate`, `generate-token`, `is-expired-p`
- Constants: `+email-verification+`, `+password-reset+`

**Token Registry Package:** `ningle-auth/token-registry`
- Functions: `register-token-purpose`, `token-purpose-valid-p`
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
