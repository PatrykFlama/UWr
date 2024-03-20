import { useState } from "react";
import useRecipe from "../providers/RecipeProvider/useRecipe";

export default function AddRecipe() {
  const [title, setTitle] = useState("");
  const [ingredients, setIngredients] = useState<string[]>([]);
  const [steps, setSteps] = useState<string[]>([]);
  const [error, setError] = useState(false);
  const { addRecipe } = useRecipe();

  return (
    <div
      className="add-recipe"
      onSubmit={(e) => {
        e.preventDefault();
        if (title.trim() === "" || ingredients.length <= 0 || steps.length <= 0) {
          setError(true);
          return;
        }
        addRecipe(title, 
                  ingredients.filter((i) => i.trim() !== "").map((i) => i.trim()), 
                  steps.filter((i) => i.trim() !== "").map((i) => i.trim()));
        setTitle("");
        setIngredients([]);
        setSteps([]);
        setError(false);
      }}
    >
      <form>
        <input
          type="text"
          placeholder="Title"
          value={title}
          onChange={(e) => setTitle(e.target.value)}
        />
        <textarea
          placeholder="Ingredients separated by comma"
          value={ingredients.join(',')}
          onChange={(e) => setIngredients(e.target.value.split(','))}
        />
        <textarea
          placeholder="Steps separated by newlines"
          value={steps.join('\n')}
          onChange={(e) => setSteps(e.target.value.split('\n'))}
        />
        <button type="submit">Add Recipe</button>
        {error && <p className="error">Error</p>}
      </form>
    </div>
  );
}