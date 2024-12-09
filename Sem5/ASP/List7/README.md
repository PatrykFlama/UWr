(wróć)[../]

# Lista 7
| 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|
| X | X |   |   |   |

## Pytania
* zad2 - czy dobry podział na foldery?
* jaki jest najpopularniejszy/standard?

## Zadanie 2
* dlaczego po stronie serwera hasła użytkownika nie można przechować w postaci jawnej?  
bruh

* dlaczego niektóre funkcje skrótu (które?) są niewskazane w praktyce?  


* do czego potrzebna jest dodatkowa wartość (salt) przy wyliczaniu skrótu?  
losowa wartość potrzebna do policzenia hasha, inaczej takie same hasła miałyby taki sam hash

* dlaczego hasła przechowuje się w osobnej tabeli i nie wystarczy do tego kolumna (kolumny) w tej samej tabeli w której przechowuje się listę użytkowników?  
większe bezpieczeństwo i wygoda zarządzania hasłami / metodami autentykajci

* jakie mechanizmy ochrony przed atakami typu brute-force można zastosować w typowej aplikacji internetowej?  
liczba nieudanych prób logowania, captcha, 2FA

* jak obsłużyć scenariusz w którym użytkownik zapomniał hasła i chce je w jakiś sposób odzyskać?  
email z resetem hasła, kontakt z administratorem, inne metody logowania

## Zadanie 3
<section>
	<summary> asp.net framework </summary>

główny web.config
```xml
<configuration>
  <location path="AdminPage.aspx">
    <system.web>
      <authorization>
        <allow roles="Admin" />
        <deny users="*" />
      </authorization>
    </system.web>
  </location>
</configuration>
```

web.config w folderze z chronionymi komponentami
```xml
<configuration>
  <system.web>
    <authorization>
      <allow roles="Admin" />
      <deny users="*" />
    </authorization>
  </system.web>
</configuration>
```

RoleProvider
```cs
public class CustomRoleProvider : RoleProvider
{
    public override string[] GetRolesForUser(string username)
    {
        using (var context = new AppDbContext())
        {
            var user = context.Users.Include(u => u.UserRoles)
                .ThenInclude(ur => ur.Role)
                .FirstOrDefault(u => u.UserName == username);

            return user?.UserRoles.Select(ur => ur.Role.RoleName).ToArray() ?? new string[] { };
        }
    }

    public override bool IsUserInRole(string username, string roleName) => throw new NotImplementedException();
    public override string ApplicationName { get; set; }
    public override void CreateRole(string roleName) => throw new NotImplementedException();
    public override bool DeleteRole(string roleName, bool throwOnPopulatedRole) => throw new NotImplementedException();
    public override string[] FindUsersInRole(string roleName, string usernameToMatch) => throw new NotImplementedException();
    public override string[] GetAllRoles() => throw new NotImplementedException();
    public override string[] GetUsersInRole(string roleName) => throw new NotImplementedException();
    public override bool RoleExists(string roleName) => throw new NotImplementedException();
}
```

</section>