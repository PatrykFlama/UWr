import { IRecipe } from "../../types/Recipe.type";
import useRecipe from "../../providers/RecipeProvider/useRecipe";

interface IProps {
    recipe: IRecipe;
}

export default function RecipeItem({recipe}: IProps) {
    const { removeRecipe, toggleFavourite } = useRecipe();

    return (
        <div className='recipe-item' key={recipe.id}>
            <h3>{recipe.title}</h3>
            <p>{recipe.ingredients.join(', ')}</p>
            <span>{recipe.steps.join('\n')}</span>      {/* //TODO why it is not adding newlines in the effect code */}
            <button onClick={() => removeRecipe(recipe.id)}>Delete</button>
            <button onClick={() => toggleFavourite(recipe.id)}>
                {recipe.is_favourite ? "Remove from favourites" : "Add to favourites"}
            </button>
        </div>
    )
}
