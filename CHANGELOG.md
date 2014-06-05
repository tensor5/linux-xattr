Version 0.1.1.0 (2014-06-05)
----------------------------

- New functions for working in the `user` namespace.
    * Set extended `user` attributes:
        - setUserXAttr
        - lSetUserXAttr
        - fdSetUserXAttr
    * Create extended `user` attributes:
        - createUserXAttr
        - lCreateUserXAttr
        - fdCreateUserXAttr
    * Replace extended `user` attributes:
        - replaceUserXAttr
        - lReplaceUserXAttr
        - fdReplaceUserXAttr
    * Retrieve extended `user` attributes:
        - getUserXAttr
        - lGetUserXAttr
        - fdGetUserXAttr
    * List extended `user` attributes:
        - listUserXAttr
        - lListUserXAttr
        - fdListUserXAttr
    * Remove extended `user` attributes:
        - removeUserXAttr
        - lRemoveUserXAttr
        - fdRemoveUserXAttr
- Use Safe Haskell extension.
- Use type synonyms `Name` and `Value` for name and value of extended
  attributes.
- Improve documentation.
- Minor code fixes.

Version 0.1.0.1 (2014-04-08)
----------------------------

- Relicense under BSD3.
- Edit description and .cabal file.
- Clean up code.

Version 0.1.0.0 (2013-03-15)
----------------------------

- Initial release of `linux-xattr`, previously called `XAttr`.
