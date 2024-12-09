using DotNetOpenAuth.OAuth2;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Zad4.Google
{
    public class MyAuthorizationTracker : IClientAuthorizationTracker
    {

        public IAuthorizationState GetAuthorizationState(
            Uri callbackUrl,
            string clientState)
        {
            return new AuthorizationState
            {
                Callback = new Uri(callbackUrl.GetLeftPart(UriPartial.Path))
            };
        }

    }

}