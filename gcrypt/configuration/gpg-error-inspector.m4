dnl (foreign crypto gpg-error sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Sat Dec 26, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl


NAUSICAA_INSPECT_TYPE([GPG_ERROR_T],[gpg_error_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([GPG_ERR_SOURCE_T],[gpg_err_source_t],[signed-int],[#f])

dnl enum gpg_err_source_t
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_UNKNOWN])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GCRYPT])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GPG])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GPGSM])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GPGAGENT])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_PINENTRY])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_SCD])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GPGME])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_KEYBOX])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_KSBA])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_DIRMNGR])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GSTI])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_GPA])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_KLEO])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_ANY])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_USER_1])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_USER_2])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_USER_3])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_USER_4])
NAUSICAA_ENUM_VALUE([GPG_ERR_SOURCE_DIM])
NAUSICAA_INSPECT_TYPE([GPG_ERR_CODE_T],[gpg_err_code_t],[signed-int],[#f])

dnl enum gpg_err_code_t
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_ERROR])
NAUSICAA_ENUM_VALUE([GPG_ERR_GENERAL])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_PACKET])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_VERSION])
NAUSICAA_ENUM_VALUE([GPG_ERR_PUBKEY_ALGO])
NAUSICAA_ENUM_VALUE([GPG_ERR_DIGEST_ALGO])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_PUBKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_SECKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_SIGNATURE])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_PUBKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_CHECKSUM])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_PASSPHRASE])
NAUSICAA_ENUM_VALUE([GPG_ERR_CIPHER_ALGO])
NAUSICAA_ENUM_VALUE([GPG_ERR_KEYRING_OPEN])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_PACKET])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_ARMOR])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_USER_ID])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_SECKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_WRONG_SECKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_KEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_COMPR_ALGO])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_PRIME])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_ENCODING_METHOD])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_ENCRYPTION_SCHEME])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_SIGNATURE_SCHEME])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_ATTR])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_VALUE])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_FOUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_VALUE_NOT_FOUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_SYNTAX])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_MPI])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_PASSPHRASE])
NAUSICAA_ENUM_VALUE([GPG_ERR_SIG_CLASS])
NAUSICAA_ENUM_VALUE([GPG_ERR_RESOURCE_LIMIT])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_KEYRING])
NAUSICAA_ENUM_VALUE([GPG_ERR_TRUSTDB])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_CERT])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_USER_ID])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNEXPECTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_TIME_CONFLICT])
NAUSICAA_ENUM_VALUE([GPG_ERR_KEYSERVER])
NAUSICAA_ENUM_VALUE([GPG_ERR_WRONG_PUBKEY_ALGO])
NAUSICAA_ENUM_VALUE([GPG_ERR_TRIBUTE_TO_D_A])
NAUSICAA_ENUM_VALUE([GPG_ERR_WEAK_KEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_KEYLEN])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_ARG])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_URI])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_URI])
NAUSICAA_ENUM_VALUE([GPG_ERR_NETWORK])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_HOST])
NAUSICAA_ENUM_VALUE([GPG_ERR_SELFTEST_FAILED])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_ENCRYPTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_PROCESSED])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNUSABLE_PUBKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNUSABLE_SECKEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_VALUE])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_CERT_CHAIN])
NAUSICAA_ENUM_VALUE([GPG_ERR_MISSING_CERT])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_DATA])
NAUSICAA_ENUM_VALUE([GPG_ERR_BUG])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_SUPPORTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_OP])
NAUSICAA_ENUM_VALUE([GPG_ERR_TIMEOUT])
NAUSICAA_ENUM_VALUE([GPG_ERR_INTERNAL])
NAUSICAA_ENUM_VALUE([GPG_ERR_EOF_GCRYPT])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_TOO_SHORT])
NAUSICAA_ENUM_VALUE([GPG_ERR_TOO_LARGE])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_IMPLEMENTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_CONFLICT])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_CIPHER_MODE])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_FLAG])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_HANDLE])
NAUSICAA_ENUM_VALUE([GPG_ERR_TRUNCATED])
NAUSICAA_ENUM_VALUE([GPG_ERR_INCOMPLETE_LINE])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_RESPONSE])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_AGENT])
NAUSICAA_ENUM_VALUE([GPG_ERR_AGENT])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_DATA])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASSUAN_SERVER_FAULT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASSUAN])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_SESSION_KEY])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_SEXP])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_ALGORITHM])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_PIN_ENTRY])
NAUSICAA_ENUM_VALUE([GPG_ERR_PIN_ENTRY])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_PIN])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_NAME])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_DATA])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_PARAMETER])
NAUSICAA_ENUM_VALUE([GPG_ERR_WRONG_CARD])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_DIRMNGR])
NAUSICAA_ENUM_VALUE([GPG_ERR_DIRMNGR])
NAUSICAA_ENUM_VALUE([GPG_ERR_CERT_REVOKED])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_CRL_KNOWN])
NAUSICAA_ENUM_VALUE([GPG_ERR_CRL_TOO_OLD])
NAUSICAA_ENUM_VALUE([GPG_ERR_LINE_TOO_LONG])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_TRUSTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_CANCELED])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_CA_CERT])
NAUSICAA_ENUM_VALUE([GPG_ERR_CERT_EXPIRED])
NAUSICAA_ENUM_VALUE([GPG_ERR_CERT_TOO_YOUNG])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_CERT])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_SEXP])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_PROTECTION])
NAUSICAA_ENUM_VALUE([GPG_ERR_CORRUPTED_PROTECTION])
NAUSICAA_ENUM_VALUE([GPG_ERR_AMBIGUOUS_NAME])
NAUSICAA_ENUM_VALUE([GPG_ERR_CARD])
NAUSICAA_ENUM_VALUE([GPG_ERR_CARD_RESET])
NAUSICAA_ENUM_VALUE([GPG_ERR_CARD_REMOVED])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_CARD])
NAUSICAA_ENUM_VALUE([GPG_ERR_CARD_NOT_PRESENT])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_PKCS15_APP])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_CONFIRMED])
NAUSICAA_ENUM_VALUE([GPG_ERR_CONFIGURATION])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_POLICY_MATCH])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_INDEX])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_ID])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_SCDAEMON])
NAUSICAA_ENUM_VALUE([GPG_ERR_SCDAEMON])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_PROTOCOL])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_PIN_METHOD])
NAUSICAA_ENUM_VALUE([GPG_ERR_CARD_NOT_INITIALIZED])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_OPERATION])
NAUSICAA_ENUM_VALUE([GPG_ERR_WRONG_KEY_USAGE])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOTHING_FOUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_WRONG_BLOB_TYPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_MISSING_VALUE])
NAUSICAA_ENUM_VALUE([GPG_ERR_HARDWARE])
NAUSICAA_ENUM_VALUE([GPG_ERR_PIN_BLOCKED])
NAUSICAA_ENUM_VALUE([GPG_ERR_USE_CONDITIONS])
NAUSICAA_ENUM_VALUE([GPG_ERR_PIN_NOT_SYNCED])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_CRL])
NAUSICAA_ENUM_VALUE([GPG_ERR_BAD_BER])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_BER])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELEMENT_NOT_FOUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_IDENTIFIER_NOT_FOUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_TAG])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_LENGTH])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_KEYINFO])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNEXPECTED_TAG])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_DER_ENCODED])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_CMS_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_CMS_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_CMS_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_CMS_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_ENCODING])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_CMS_VERSION])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_ALGORITHM])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_ENGINE])
NAUSICAA_ENUM_VALUE([GPG_ERR_PUBKEY_NOT_TRUSTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_DECRYPT_FAILED])
NAUSICAA_ENUM_VALUE([GPG_ERR_KEY_EXPIRED])
NAUSICAA_ENUM_VALUE([GPG_ERR_SIG_EXPIRED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENCODING_PROBLEM])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_STATE])
NAUSICAA_ENUM_VALUE([GPG_ERR_DUP_VALUE])
NAUSICAA_ENUM_VALUE([GPG_ERR_MISSING_ACTION])
NAUSICAA_ENUM_VALUE([GPG_ERR_MODULE_NOT_FOUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_OID_STRING])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_TIME])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_CRL_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNSUPPORTED_CRL_VERSION])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_CERT_OBJ])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_NAME])
NAUSICAA_ENUM_VALUE([GPG_ERR_LOCALE_PROBLEM])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_LOCKED])
NAUSICAA_ENUM_VALUE([GPG_ERR_PROTOCOL_VIOLATION])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_MAC])
NAUSICAA_ENUM_VALUE([GPG_ERR_INV_REQUEST])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_EXTN])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_CRIT_EXTN])
NAUSICAA_ENUM_VALUE([GPG_ERR_LOCKED])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_OPTION])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_COMMAND])
NAUSICAA_ENUM_VALUE([GPG_ERR_NOT_OPERATIONAL])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_PASSPHRASE])
NAUSICAA_ENUM_VALUE([GPG_ERR_NO_PIN])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNFINISHED])
NAUSICAA_ENUM_VALUE([GPG_ERR_BUFFER_TOO_SHORT])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_INV_LEN_SPEC])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_STRING_TOO_LONG])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_UNMATCHED_PAREN])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_NOT_CANONICAL])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_BAD_CHARACTER])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_BAD_QUOTATION])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_ZERO_PREFIX])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_NESTED_DH])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_UNMATCHED_DH])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_UNEXPECTED_PUNC])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_BAD_HEX_CHAR])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_ODD_HEX_NUMBERS])
NAUSICAA_ENUM_VALUE([GPG_ERR_SEXP_BAD_OCT_CHAR])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_GENERAL])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_ACCEPT_FAILED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_CONNECT_FAILED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_INV_RESPONSE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_INV_VALUE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_INCOMPLETE_LINE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_LINE_TOO_LONG])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NESTED_COMMANDS])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NO_DATA_CB])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NO_INQUIRE_CB])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NOT_A_SERVER])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NOT_A_CLIENT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_SERVER_START])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_READ_ERROR])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_WRITE_ERROR])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_TOO_MUCH_DATA])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_UNEXPECTED_CMD])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_UNKNOWN_CMD])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_SYNTAX])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_CANCELED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NO_INPUT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_NO_OUTPUT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_PARAMETER])
NAUSICAA_ENUM_VALUE([GPG_ERR_ASS_UNKNOWN_INQUIRE])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_1])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_2])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_3])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_4])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_5])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_6])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_7])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_8])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_9])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_10])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_11])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_12])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_13])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_14])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_15])
NAUSICAA_ENUM_VALUE([GPG_ERR_USER_16])
NAUSICAA_ENUM_VALUE([GPG_ERR_MISSING_ERRNO])
NAUSICAA_ENUM_VALUE([GPG_ERR_UNKNOWN_ERRNO])
NAUSICAA_ENUM_VALUE([GPG_ERR_EOF])
NAUSICAA_ENUM_VALUE([GPG_ERR_E2BIG])
NAUSICAA_ENUM_VALUE([GPG_ERR_EACCES])
NAUSICAA_ENUM_VALUE([GPG_ERR_EADDRINUSE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EADDRNOTAVAIL])
NAUSICAA_ENUM_VALUE([GPG_ERR_EADV])
NAUSICAA_ENUM_VALUE([GPG_ERR_EAFNOSUPPORT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EAGAIN])
NAUSICAA_ENUM_VALUE([GPG_ERR_EALREADY])
NAUSICAA_ENUM_VALUE([GPG_ERR_EAUTH])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBACKGROUND])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADF])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADFD])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADMSG])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADR])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADRPC])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADRQC])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBADSLT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBFONT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EBUSY])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECANCELED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECHILD])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECHRNG])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECOMM])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECONNABORTED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECONNREFUSED])
NAUSICAA_ENUM_VALUE([GPG_ERR_ECONNRESET])
NAUSICAA_ENUM_VALUE([GPG_ERR_ED])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDEADLK])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDEADLOCK])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDESTADDRREQ])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDIED])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDOM])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDOTDOT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EDQUOT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EEXIST])
NAUSICAA_ENUM_VALUE([GPG_ERR_EFAULT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EFBIG])
NAUSICAA_ENUM_VALUE([GPG_ERR_EFTYPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EGRATUITOUS])
NAUSICAA_ENUM_VALUE([GPG_ERR_EGREGIOUS])
NAUSICAA_ENUM_VALUE([GPG_ERR_EHOSTDOWN])
NAUSICAA_ENUM_VALUE([GPG_ERR_EHOSTUNREACH])
NAUSICAA_ENUM_VALUE([GPG_ERR_EIDRM])
NAUSICAA_ENUM_VALUE([GPG_ERR_EIEIO])
NAUSICAA_ENUM_VALUE([GPG_ERR_EILSEQ])
NAUSICAA_ENUM_VALUE([GPG_ERR_EINPROGRESS])
NAUSICAA_ENUM_VALUE([GPG_ERR_EINTR])
NAUSICAA_ENUM_VALUE([GPG_ERR_EINVAL])
NAUSICAA_ENUM_VALUE([GPG_ERR_EIO])
NAUSICAA_ENUM_VALUE([GPG_ERR_EISCONN])
NAUSICAA_ENUM_VALUE([GPG_ERR_EISDIR])
NAUSICAA_ENUM_VALUE([GPG_ERR_EISNAM])
NAUSICAA_ENUM_VALUE([GPG_ERR_EL2HLT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EL2NSYNC])
NAUSICAA_ENUM_VALUE([GPG_ERR_EL3HLT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EL3RST])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELIBACC])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELIBBAD])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELIBEXEC])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELIBMAX])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELIBSCN])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELNRNG])
NAUSICAA_ENUM_VALUE([GPG_ERR_ELOOP])
NAUSICAA_ENUM_VALUE([GPG_ERR_EMEDIUMTYPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EMFILE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EMLINK])
NAUSICAA_ENUM_VALUE([GPG_ERR_EMSGSIZE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EMULTIHOP])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENAMETOOLONG])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENAVAIL])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENEEDAUTH])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENETDOWN])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENETRESET])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENETUNREACH])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENFILE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOANO])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOBUFS])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOCSI])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENODATA])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENODEV])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOENT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOEXEC])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOLCK])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOLINK])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOMEDIUM])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOMEM])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOMSG])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENONET])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOPKG])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOPROTOOPT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOSPC])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOSR])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOSTR])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOSYS])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTBLK])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTCONN])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTDIR])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTEMPTY])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTNAM])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTSOCK])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTSUP])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTTY])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENOTUNIQ])
NAUSICAA_ENUM_VALUE([GPG_ERR_ENXIO])
NAUSICAA_ENUM_VALUE([GPG_ERR_EOPNOTSUPP])
NAUSICAA_ENUM_VALUE([GPG_ERR_EOVERFLOW])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPERM])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPFNOSUPPORT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPIPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROCLIM])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROCUNAVAIL])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROGMISMATCH])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROGUNAVAIL])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROTO])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROTONOSUPPORT])
NAUSICAA_ENUM_VALUE([GPG_ERR_EPROTOTYPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ERANGE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EREMCHG])
NAUSICAA_ENUM_VALUE([GPG_ERR_EREMOTE])
NAUSICAA_ENUM_VALUE([GPG_ERR_EREMOTEIO])
NAUSICAA_ENUM_VALUE([GPG_ERR_ERESTART])
NAUSICAA_ENUM_VALUE([GPG_ERR_EROFS])
NAUSICAA_ENUM_VALUE([GPG_ERR_ERPCMISMATCH])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESHUTDOWN])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESOCKTNOSUPPORT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESPIPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESRCH])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESRMNT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESTALE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ESTRPIPE])
NAUSICAA_ENUM_VALUE([GPG_ERR_ETIME])
NAUSICAA_ENUM_VALUE([GPG_ERR_ETIMEDOUT])
NAUSICAA_ENUM_VALUE([GPG_ERR_ETOOMANYREFS])
NAUSICAA_ENUM_VALUE([GPG_ERR_ETXTBSY])
NAUSICAA_ENUM_VALUE([GPG_ERR_EUCLEAN])
NAUSICAA_ENUM_VALUE([GPG_ERR_EUNATCH])
NAUSICAA_ENUM_VALUE([GPG_ERR_EUSERS])
NAUSICAA_ENUM_VALUE([GPG_ERR_EWOULDBLOCK])
NAUSICAA_ENUM_VALUE([GPG_ERR_EXDEV])
NAUSICAA_ENUM_VALUE([GPG_ERR_EXFULL])
NAUSICAA_ENUM_VALUE([GPG_ERR_CODE_DIM])

dnl Preprocessor symbols: miscellaneous constants
NAUSICAA_DEFINE_VALUE([GPG_ERR_CODE_MASK])
NAUSICAA_DEFINE_VALUE([GPG_ERR_SOURCE_MASK])
NAUSICAA_DEFINE_VALUE([GPG_ERR_SOURCE_SHIFT])
NAUSICAA_DEFINE_VALUE([GPG_ERR_INITIALIZED])
NAUSICAA_DEFINE_VALUE([GPG_ERR_SOURCE_DEFAULT])
NAU_DS_WITH_OPTION([GPG_ERROR_SHARED_OBJECT],[gpg-error-shared-object],[libgpg-error.so],
  [Gpg-Error shared library file],[select Gpg-Error shared library file])

dnl end of file
