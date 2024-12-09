using DotNetOpenAuth.OAuth2;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Zad4.Google
{
    public class GoogleClient : WebServerClient
    {
        private static readonly AuthorizationServerDescription GoogleDescription =
            new AuthorizationServerDescription
            {
                TokenEndpoint = new Uri("https://accounts.google.com/o/oauth2/token"),
                AuthorizationEndpoint = new Uri("https://accounts.google.com/o/oauth2/auth"),
                ProtocolVersion = ProtocolVersion.V20,
            };

        public const string ProfileEndpoint = "https://www.googleapis.com/plus/v1/people/me/openIdConnect";

        public const string OpenId = "openid";
        public const string ProfileScope = "profile";
        public const string EmailScope = "email";

        public GoogleClient()
            : base(GoogleDescription)
        {
        }
    }


}