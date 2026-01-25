# lista 8
Zadania:

1. Zainstaluj dowolny serwer pocztowy (polecam np. Postfix) (1p)
2. Skonfiguruj serwer pocztowy i domenę tak, żeby działały machanizmy antyspam, SPF oraz DKIM (⇒DMARC), DMARC ma mieć ustawioną politykę “p=reject”. (polecam np. rpsamd) (8p)
3. Doinstaluj dowolny serwer IMAP (polecam np. Dovecot) i skonfiguruj konto pocztowe w swojej domenie w dowolnym programie pocztowym (może to być nawet gmail!) (2p)
4. Wyślij mail na adres check-auth@verifier.port25.com. Po jakimś czasie powinna przyjść odpowiedź. Ta odopwiedź powinna zawierać:

```
==========================================================
Summary of Results
==========================================================
SPF check:          pass
DKIM check:         pass
```

5. Wyślij mail do dowolnego konta gmail do którego masz dostęp i sprawdź nagłówki wiadomośći (“show original”). Powinny być mniej-więcej takie:
```
ARC-Authentication-Results: i=1; mx.google.com;
       dkim=pass header.i=@twojadomena.tld header.s=mail header.b=JMYixQto;
       spf=pass (google.com: domain of user@twojadomena.tld designates <IP> as permitted sender) smtp.mailfrom=user@twojadomena.tld;
       dmarc=pass (p=REJECT sp=REJECT dis=NONE) header.from=twojadomena.tld
```

6. Doprowadź by testy (1) https://www.mythic-beasts.com/ipv6/health-check oraz (2) https://mxtoolbox.com/domain/ pokazywały: (1) - 11/11, (2) - im więcej zielonego tym lepiej ;) (2p)
7. Dodaj do swojej konfiguracji ARC, MTA-STS i TLS-RPT - sprawdź wyniki na stronie https://www.hardenize.com/ (3p)

