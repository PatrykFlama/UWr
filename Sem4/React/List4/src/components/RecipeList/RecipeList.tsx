import useRecipe from "../../providers/RecipeProvider/useRecipe";
import RecipeItem from "./RecipeItem";

interface IProps {
    onlyFavourites: boolean;
    filterString: string;
}

export default function RecipeList({ onlyFavourites, filterString }: IProps ) {
    const { recipes } = useRecipe();

    return (
        <div className="recipe-list">
            {recipes
                .filter((recipe) => (
                    (!onlyFavourites || recipe.is_favourite) && 
                    (recipe.title.toLowerCase().includes(filterString.toLowerCase()) || 
                     recipe.ingredients.some(ingredient => ingredient.toLowerCase().includes(filterString.toLowerCase())) ||
                     recipe.steps.some(step => step.toLowerCase().includes(filterString.toLowerCase())))))
                .map((recipe) => (
                    <RecipeItem recipe={recipe} />
            ))}
        </div>
    );
};