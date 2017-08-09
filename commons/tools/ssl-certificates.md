---
title: Installing TLS Certificates
position: 3
shortdesc: How to prepare your TLS certificates on Clever Cloud
tags:
- ssl
- tls
- certificate
- https
---

# Installing TLS Certificates

TLS certificates are necessary to enable secure HTTPS connections between a browser and you application. These certificates can be bought online, or directly via Clever Cloud (Domain Validation or Wildcard SSL). In that case, we manage the whole process for you.

If you already have your certificates, you can upload them on Clever Cloud. 

## Uploading certificates

You can upload certificates yourself: <https://api.clever-cloud.com/v2/certificates/new>

You need to paste a PEM bundle containing (in this order):

 - the private key
 - the certificate itself
 - intermediate certificates

## Transmitting your files manually

If you need manual operations on the certificates, the most secure way to transfer your certificates is using a signed and encrypted email.
Our dedicated email for receiving certificates is [ssl@clever-cloud.com](mailto:ssl@clever-cloud.com).

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4 class="panel-title">Security</h4>
  </div>
  <div class="panel-body">
    Do not send any of these files via an unsecure way. The integrity of your certificate may not be guaranteed.
  </div>
</div>

### Clever Cloud on Keybase

If your are a Keybase.io user, you can find us at [keybase.io/clevercloud](https://keybase.io/clevercloud).

### Clever Cloud's Public Key (for GPG users)

* fingerprint:  `03943517934C1FA5ED4E2F61218B86BD5278460F`
* 64-bit: `218B86BD5278460F`

```bash
-----BEGIN PGP PUBLIC KEY BLOCK-----
Comment: GPGTools - https://gpgtools.org

mQINBFa4bJMBEAC21vsfJ1ay5iVUcKsP8X8GziZu8daV5G4Lqpns54zN/GB05f9+
3jV1RYRMweq7RC6XU/GDZo20ksvDw0N963/WCswCt0MyzM1O105tZ/ZbYyVV/w5w
763DXCUPdBhzcA7o31A7JgPimiceP/BcyA8CniLSD4nfzD0CiM74kdG5/vx2Ve9T
vkJ0wX5qWFvEivBPf/+jUfmdwtx5RLjH783xRuh0wWcz9ey/hsuLxJmdDuIlUccu
yH0itAMiLmI0yGscTVQ6pmLmEY1GZY3GSrTZbDOrKbJLxPjtRAtW8JvE1UD9AOqs
yvJ+QYp+6viEkQg0Fq2agCkpWTEy/6INGvjwexHkq9zUBe/Dm0awcqg46GSY8cwA
gqaaV32hPwv4fYMsz8fH50DDIAmKjJspWTeehnaXUSmr9lHpYH/l1mHPOtCvBL2e
m52YAKF5bCQEwn1p/UsysxtTaOrLO3ZoRTpM9lAeR3SfM768lnLzpI2AcpyJ95jg
btibuy59WvCQMdbA6UneaWxaOhSAtAB3OzHauXhQzru1BXfy5VOva5YHgH+4xS9n
7aqMxG0uB9X7wcPPV9FHFv+6QnKZ1fHLf6zN0tUjoXTIS6egbJqtYcTbDE8nRgoU
rwEadolEVSNnTEX3cfdueUHslMZwF+U30RG2bHSJTmUTOnVT6g1zk2hxwwARAQAB
tD5DbGV2ZXIgQ2xvdWQgKENsZXZlciBDbG91ZCdzIFNTTCBlbWFpbCkgPHNzbEBj
bGV2ZXItY2xvdWQuY29tPokCPQQTAQoAJwUCVrhskwIbAwUJB4YfgAULCQgHAwUV
CgkICwUWAgMBAAIeAQIXgAAKCRAhi4a9UnhGDwbQD/49Ol6HlYivxbxHi+uZdI2V
RoRLQlbTjrpIWc3ENJSjo19w7ntirsTkAmwh80Uvf72dLTPT3/dAa7qDiLHbaEPq
1Qr9XWgvb1rs/iXaJyEG/bhPEYoB/cvV+C921vArx6R8ZaUV0GZnZraOQgXCXq/y
a7QdM+wnh2abRwEOUFqVc2q2/N/1SjCKx9cCk2v4s3/chC/EZfUw3AoDQSwRkKYk
JD8r5r7TP7m048KreIKL5YNh9M6ybfsXPAtVRT3qIXFFDhrTcTvRokf50EFDwGVC
7miSaDDBLzQOJaaAD4j7I+QsiiKJSE41bymPcNNtZ/StILVTtmUSWSZAYC5Ke/PI
78o/0Hp3vT2WUw3w4iAHhbDhaPk+swC1rzTvPUXNb0ELVZb23ouPGqn0gMFcv0I5
ojQ+hH/J5kQ2VvRUc7aCzA6Cqqavn9FljlwbI/Vro562GTkHQnmvKAnTYLomhFyL
TLrCs8vnLIVBJiSO4v/di6GhderDGjeeEgKXN+BuxU+V1u4kTp9uRjlNtEHoGPo7
n38nChtFmRkfCp8eBXKDTZllxHUoiBSbEpY64zr3D3X9KTRBbS+JJXFpTcHJXeC2
Ifc+5rOeJFjjmQ7iui0u0wwGaXY5WaQYd7zQ2rQz8pDaINsOAMTdaRJ5rPFBmAAn
KX7GBYRhPwWB0sjSHhRttLkCDQRWuGyTARAAp2fLCM77rmEREyt4Rn2Psd+RU6Ad
4k5Zlug6JE0BC9H340RKo3ViXV258Yqg2ra8rr9anJ4qX+T5ZsgVQ6daRstE7l9N
d62w+86ZcI+av3ncAKAihZsuZeZiI4NldCFoaJUu2Ixt/Bk4ppY+Uo0MkdL3Rq/6
pEt4WGjOm+KMY38mSYGgzkdyOrncZ8+XY8UFvza/MAu4ukuduh+uozXRvCiaeiEx
4OhPR4TksZ6RrP++f/ZywTf19Qn3/7ickW4TU7F/khGMg+xtTkgFA+pdes2JrfgF
G7zvLIsQqfB7rNTNPHaQhazdQDWX9ylzg+Az/uoF3nMcEgLdawm2X60cyzo0ogt1
F1f+juMvJJ+W7ao7Dfve0qoSQtEOmmR8sc4vVBzdPAFCOh8QYqm1z4JGbDcmWfG4
ypGbBXBiGLyeJRm3o4iRBkAl7jkSNJeDnO4ajrTmZYSpO/NecbWAiybIxpoqQ3Yy
36XyTJJp6sie/6BWEF+tJUC4w4jQiuGeE+As9VwkGnsH0+m9gOwiO/TUocrxOHal
W2GB6s9V2zBqMFHEwKZgGZXLMG7IbiHS72QnrynwZpkAW328SUb/DgkI5DbbeVDG
tFNHBDiqxFgREv7c8DpnnJK7WX59HAZlXDcz1jfc6oKCFILDB0ujfuceRMGCxZ7m
Slkp3cHibEvsW18AEQEAAYkCJQQYAQoADwUCVrhskwIbDAUJB4YfgAAKCRAhi4a9
UnhGD1zuD/99suvRucY3IJXHFCsV4wHIVgerU86sx3w0Nu5p18feqEf6K9tODZ32
NcFKaWKVZ7tK1G0fzOnoLiT/Pzhu2pjq+cFN9t1CQy5U2cyFRRbrkd64LsIU6Nln
BVLNTje+akWnF3ezOSYvU02LElt4HQgTE1fMh7lolNcDrsg3hAd7vWJsr5r9MNtL
hxKrRjT1P+5op+lomSHeeWMnj2DxnwyA4es7fRfHcqC6fCW39Plhl6uCGaKNaR4n
wr4ht/n06QsBcGyIJrrkTrRGiHx9z9McNdI3hGZijBFYtPGfSsDFtipf8sOXIok3
rQ3NGoRYb9siJKupaMNXhhr8awXX36DlWIoS6m0pnlD9F3ZC+iWMQMpAFTg/8+ZU
IfFBXZpXGw8WLiLNiBWcxVrgPDNm0IJFoOd52fyFRyDGukhJFUJXWyQK2bKvOZdk
jUWMNJ2sQTFMlIAnhgeWnalMU4GOgSumxGozZ7fYGQJdPdclIrMSJFdHghsaX5PM
lxA/k5PoRGfhx2p+REbdgvD3oN11Kep2Y6/PEC9n0wPU9VHk+R8Ab9jLekQjcfLB
bvoe/dzJcoT/thvBKpxgRCoEFI0ozYUnU9L288hPg3ctLR6k8e7ALMbPk13BFbJa
S1G9NpEsu94rDq7yehVOpGv0bCav2xtDIAgQW+ZRpRoipu5KSsUAzg==
=DNbu
-----END PGP PUBLIC KEY BLOCK-----
```