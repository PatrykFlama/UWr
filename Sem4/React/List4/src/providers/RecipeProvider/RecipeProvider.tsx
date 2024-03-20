import { createContext, useReducer } from "react";
import { IRecipe } from "../../types/Recipe.type";
import { recipeReducer } from "./recipeReducer";

export const RecipeContext = createContext<{
  recipes: IRecipe[];
  addRecipe: (title: string, ingredients: string[], steps: string[]) => void;
  removeRecipe: (id: number) => void;
  toggleFavourite: (id: number) => void;
}>({
  recipes: [],
  addRecipe: () => {},
  removeRecipe: () => {},
  toggleFavourite: () => {},
});

const initialRecipes: IRecipe[] = [
{
    id: 1,
    title: "Recipe 1",
    ingredients: ["Ingredient 1", "Ingredient 2", "Ingredient 3"],
    steps: ["Step 1", "Step 2", "Step 3"],
    is_favourite: false,
  },
  {
    id: 2,
    title: "Recipe 2",
    ingredients: ["Ingredient 4", "Ingredient 5", "Ingredient 6"],
    steps: ["Step 4", "Step 5", "Step 6"],
    is_favourite: false,
  },
  {
    id: 3,
    title: "Recipe 3",
    ingredients: ["Ingredient 7", "Ingredient 8", "Ingredient 9"],
    steps: ["Step 7", "Step 8", "Step 9"],
    is_favourite: false,
}
];

const RecipeProvider = ({ children }: { children: React.ReactNode }) => {
  const [recipes, dispatchRecipe] = useReducer(recipeReducer, initialRecipes);

  function addRecipe(title: string, ingredients: string[], steps: string[]) {
    dispatchRecipe({ type: "ADD", payload: { title, ingredients, steps } });
  }

  function removeRecipe(id: number) {
    dispatchRecipe({ type: "REMOVE", payload: { id } });
  }

  function toggleFavourite(id: number) {
    dispatchRecipe({ type: "TOGGLE_FAVOURITE", payload: { id }});
  }

  return (
    <RecipeContext.Provider value={{ recipes, addRecipe, removeRecipe, toggleFavourite }}>
      {children}
    </RecipeContext.Provider>
  );
};

export default RecipeProvider;
