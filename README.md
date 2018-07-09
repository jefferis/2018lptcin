# 2018lptcin

Analysis of HS - LPTC interactions / morphology

Started GJ/EM/KS during GJ's summer 2018 sabbatical visit

Requires installation of https://github.com/jefferis/elmr
including setup of CATMAID authentication credentials as described at
https://github.com/jefferis/rcatmaid#authentication

## Mirroring

Mirroring neurons requires installation of CMTK. Moving between template brains
beyond FAFB/JFRC2013 requires you to download optional bridging registrations.

In R:

```
nat.flybrains::download_jefferislab_registrations()
```
Occasionally I have noticed issues with SSL certificates and github when doing
this. Ask on the [nat-user](https://groups.google.com/forum/embed/?place=forum/nat-user#!forum/nat-user) google group if this happens - a SSL library update is
required.
