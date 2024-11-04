/*
According to MSDN there is no way to determine cookies is allowed or disabled by user. The only way to find it out is through writing it and then reading it.

Check the section "Determining Whether a Browser Accepts Cookies" here.

It also has examples which show how to read and write cookies, and states:

The Cookies property does not indicate whether cookies are enabled. It indicates only whether the current browser inherently supports cookies.

https://learn.microsoft.com/en-us/previous-versions/ms178194(v=vs.140)?redirectedfrom=MSDN
*/

namespace Zad2
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();


            app.MapGet("/", async context => {
                await context.Response.WriteAsync($@"
                    <html>
                        <body>
                            <h2>Cookie Demo</h2>
                            <p>
                                <a href=""/set-cookie"">Set Cookie</a><br>
                                <a href=""/read-cookie"">Read Cookie</a><br>
                                <a href=""/delete-cookie"">Delete Cookie</a><br>
                                <a href=""/check-cookie-support"">Check Cookie Support</a>
                            </p>
                        </body>
                    </html>
                ");
            });

            app.MapGet("/set-cookie", context => {
                context.Response.Cookies.Append("exampleCookie", "Hello World", new CookieOptions {
                    HttpOnly = true,
                    Secure = false
                });
                return context.Response.WriteAsync("Cookie has been set");
            });

            app.MapGet("/read-cookie", context => {
                if(context.Request.Cookies.TryGetValue("exampleCookie", out var cookieValue)) {
                    return context.Response.WriteAsync($"Cookie value: {cookieValue}");
                } else {
                    return context.Response.WriteAsync("Cookie not found");
                }
            });

            app.MapGet("/delete-cookie", context => {
                context.Response.Cookies.Delete("exampleCookie");
                return context.Response.WriteAsync("Cookie has been deleted");
            });

            app.MapGet("/check-cookie-support", async context => {
                // to test cookie support we will set some cookie and check if we can read from it
                context.Response.Cookies.Append("testCookie", "testValue", new CookieOptions { 
                    Expires = DateTimeOffset.Now.AddMinutes(1) 
                });
                context.Response.Redirect("/verify-cookie-support");
            });

            app.MapGet("/verify-cookie-support", context => {
                // check if cookie eixsts
                if(context.Request.Cookies.ContainsKey("testCookie")) {
                    return context.Response.WriteAsync("Cookies supported");
                } else {
                    return context.Response.WriteAsync("Cookies not supported");
                }
            });


            app.Run();
        }
    }
}
