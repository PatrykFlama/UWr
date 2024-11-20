using Microsoft.AspNetCore.Html;
using Microsoft.AspNetCore.Mvc.Rendering;
using Microsoft.AspNetCore.Mvc.ViewFeatures;
using System.Linq.Expressions;


namespace Zad3.HtmlHelperExtensions
{
	public static class HtmlHelperExtensions
	{
		// wiązanie nazw pól tekstowych wprost
		public static IHtmlContent Login(this IHtmlHelper htmlHelper, string userNameField, string passwordField)
		{
			var userNameInput = $"<input type=\"text\" name=\"{userNameField}\" />";
			var passwordInput = $"<input type=\"text\" name=\"{passwordField}\" />";
			return new HtmlString($"{userNameInput}\n{passwordInput}");
		}

		// wiązanie przez składowe modelu
		public static IHtmlContent LoginFor<TModel>(
			this IHtmlHelper<TModel> htmlHelper,
			Expression<Func<TModel, object>> userNameExpression,
			Expression<Func<TModel, object>> passwordExpression)
		{
			var expressionProvider = htmlHelper.ViewContext.HttpContext.RequestServices
				.GetService(typeof(ModelExpressionProvider)) as ModelExpressionProvider;

			var userNameName = expressionProvider.GetExpressionText(userNameExpression);
			var passwordName = expressionProvider.GetExpressionText(passwordExpression);

			var userNameInput = $"<input type=\"text\" name=\"{userNameName}\" />";
			var passwordInput = $"<input type=\"text\" name=\"{passwordName}\" />";
			return new HtmlString($"{userNameInput}\n{passwordInput}");
		}
	}
}
