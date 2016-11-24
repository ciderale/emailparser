# Parsing & Validation of Email Addresses

- This is an experiment using the parsec library
- The email format description is taken from Wikipedia
    https://en.wikipedia.org/wiki/Email_address
- The implementation is incomplete and may differ from the RFCs.

## Syntax (https://en.wikipedia.org/wiki/Email_address)

The format of email addresses is local-part@domain where the local part may be up to 64 characters long and the domain may have a maximum of 255 characters[2]—but the maximum of 256-character length of a forward or reverse path restricts the entire email address to be no more than 254 characters long.[3] The formal definitions are in RFC 5322 (sections 3.2.3 and 3.4.1) and RFC 5321—with a more readable form given in the informational RFC 3696[4] and the associated errata.

### Local Part

The local-part of the email address may use any of these ASCII characters:

* uppercase and lowercase Latin letters A to Z and a to z;
* digits 0 to 9;
* special characters !#$%&'*+-/=?^_`{|}~;
* dot ., provided that it is not the first or last character unless quoted,
  and provided also that it does not appear consecutively unless quoted (e.g.
  John..Doe@example.com is not allowed but "John..Doe"@example.com is
  allowed);
* space and "(),:;<>@[\] characters are allowed with restrictions (they are
  only allowed inside a quoted string, as described in the paragraph below,
  and in addition, a backslash or double-quote must be preceded by a
  backslash);
* comments are allowed with parentheses at either end of the local-part; e.g.
  john.smith(comment)@example.com and (comment)john.smith@example.com are both
  equivalent to john.smith@example.com.

* In addition to the above ASCII characters, international characters above
  U+007F, encoded as UTF-8, are permitted by RFC 6531, though mail systems may
  restrict which characters to use when assigning local-parts.

* A quoted string may exist as a dot separated entity within the local-part,
  or it may exist when the outermost quotes are the outermost characters of
  the local-part (e.g., abc."defghi".xyz@example.com or
  "abcdefghixyz"@example.com are allowed.[citation needed] Conversely,
  abc"defghi"xyz@example.com is not; neither is
  abc\"def\"ghi@example.com).[citation needed] Quoted strings and characters
  however, are not commonly used.[citation needed] RFC 5321 also warns that "a
  host that expects to receive mail SHOULD avoid defining mailboxes where the
  Local-part requires (or uses) the Quoted-string form".

### Domain Part

The domain name part of an email address has to conform to strict guidelines:
it must match the requirements for a hostname, a list of dot-separated DNS
labels, each label being limited to a length of 63 characters and consisting
of:

* uppercase and lowercase Latin letters A to Z and a to z;
* digits 0 to 9, provided that top-level domain names are not all-numeric;
* hyphen -, provided that it is not the first or last character. This rule is
  known as the LDH rule (letters, digits, hyphen).

In addition, the domain may be an IP address literal, surrounded by square
brackets [], such as jsmith@[192.168.2.1] or jsmith@[IPv6:2001:db8::1],
although this is rarely seen except in email spam. Internationalized domain
names (which are encoded to comply with the requirements for a hostname) allow
for presentation of non-ASCII domains. In mail systems compliant with RFC 6531
and RFC 6532 an email address may be encoded as UTF-8, both a local-part as
well as a domain name.

Comments are allowed in the domain as well as in the local-part; for example,
john.smith@(comment)example.com and john.smith@example.com(comment) are
equivalent to john.smith@example.com
