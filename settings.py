# settings.py (GEMM)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov
# pylint: skip-file

"""
Django settings for GEMM project.

Django 2.0 info @https://docs.djangoproject.com/en/2.0/topics/settings/

Available functions:
- Set 'localhost' for development and testing
   DO NOT SETUP PRIVATE OR EPA SERVER SETTINGS!
   THESE ARE DONE ONLY ON EPA SERVERS!
"""

# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
import os

BASE_DIR = os.path.dirname(os.path.dirname(__file__))

SITE_NAME = 'gemm.epa.gov'
SITE_ID = 1
# AUTH_PROFILE_MODULE = 'accounts.UserProfile'

# Edit here to change what page is shown on user login.
# LOGIN_REDIRECT_URL = '/scenario/'
LOGIN_REDIRECT_URL = '/dashboard/'

DEFAULT_FROM_EMAIL = '__DEFAULT_FROM_EMAIL__'
EMAIL_HOST = '__EMAIL_HOST__'
EMAIL_HOST_USER = '__EMAIL_HOST_USER__'
EMAIL_HOST_PASSWORD = '__EMAIL_HOST_PASSWORD'
EMAIL_PORT = 25
EMAIL_FILE_PATH = '__EMAIL_FILE_PATH__'

WKHTMLTOPDF_CMD_OPTIONS = {
    'quiet': True,
}
if os.name != 'nt':
    WKHTMLTOPDF_CMD = '/usr/local/bin/wkhtmltopdf'
else:
    WKHTMLTOPDF_CMD = 'C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe'

# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/1.11/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = '__SECRET_KEY__'

ALLOWED_HOSTS = ['localhost', '127.0.0.1']

REST_FRAMEWORK = {
    'DEFAULT_FILTER_BACKENDS': ('django_filters.rest_framework.DjangoFilterBackend',)
}

# Application definition
INSTALLED_APPS = (
    'grappelli',
    'django.contrib.admin',
    'django.contrib.admindocs',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django.contrib.humanize',
    'django.contrib.sites',
    'accounts',
    'teams',
    # 'docs',
    'rest_framework',
    'GEMM',
    # 'session_security',
)

MIDDLEWARE = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    # 'django.contrib.auth.middleware.SessionAuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
    # 'session_security.middleware.SessionSecurityMiddleware',
    # 'app_name.session_middleware.AutoLogout',
    # 'app_name.session_middleware.timeOutMiddleware',
)

# django-session-security settings
SESSION_SECURITY_WARN_AFTER = 300
SESSION_SECURITY_EXPIRE_AFTER = 600

# True to close session when user window is close.
SESSION_SECURITY_INSECURE = True

# SECURITY WARNING: do not run with debug turned on in production!
DEBUG = True
TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
                'GEMM.context_processors.software_info',
            ],
            'debug': DEBUG,
        },
    },
]

ROOT_URLCONF = 'GEMM.urls'

WSGI_APPLICATION = 'GEMM.wsgi.application'

# Database - OVERRIDE THIS IN local_setting.py!!!
# Connection string is overridden
# https://docs.djangoproject.com/en/1.11/ref/settings/#databases
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql_psycopg2',
        'NAME': 'gemm',
        'USER': 'dyoung11',
        'PASSWORD': 'Evelynj1!',
        'HOST': 'localhost',
        'PORT': '5432'
    }
}
# Internationalization
# https://docs.djangoproject.com/en/1.11/topics/i18n/

LANGUAGE_CODE = 'en-us'
TIME_ZONE = 'America/New_York'
USE_I18N = True
USE_L10N = True
USE_TZ = True

# Session expiration
SESSION_COOKIE_AGE = 900
SESSION_SAVE_EVERY_REQUEST = True

# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/1.11/howto/static-files/
STATIC_ROOT = os.path.join(BASE_DIR, "GEMM/static")
STATIC_URL = '/static/'


DOWNLOADS_DIR = os.path.join(STATIC_ROOT, "downloads")
MANUAL_NAME = 'G-LMMD-0031134-MN-1-0.pdf'
PRIMARY_TOOL = 'GEMM_Primary 4 Chemicals_07.08.2013.xlsx'
SECONDARY_TOOL = 'GEMM_Secondary 4 Chemicals_07.08.2013.xlsx'


# MANUAL_PATH = os.path.join(STATIC_ROOT, "downloads/G-LMMD-0031134-MN-1-0.pdf")
# MANUAL_EXTENSION = 'pdf'


# MEDIA_ROOT = BASE_DIR + '/media/'
MEDIA_ROOT = os.path.join(BASE_DIR, "GEMM/media/")
MEDIA_URL = '/media/'


DOCS_ROOT = os.path.join(BASE_DIR, 'GEMM/html')
DOCS_ACCESS = 'staff'


APP_NAME = 'GEMM'
APP_VERSION = '1.1.01'
APP_DISCLAIMER = 'Neither the EPA nor any of its employees or contractors ' \
                 'makes any warranty, expressed or implied, including the ' \
                 'warranties of merchantability and fitness for a particular ' \
                 'purpose, or assumes any legal liability or responsibility ' \
                 'for the accuracy, completeness, or usefulness of any ' \
                 'information, apparatus, product, or process disclosed, or ' \
                 'represents that its use would not infringe privately owned ' \
                 'rights, with respect to documents available from this server.'


try:
    from GEMM.local_settings import *
    # pylint: disable=unused-wildcard-import,wildcard-import
except ImportError:
    pass

DEBUG_UNITTESTS = True
if DEBUG_UNITTESTS:
    import django
    django.setup()
