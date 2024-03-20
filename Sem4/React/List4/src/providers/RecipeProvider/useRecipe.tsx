import { useContext } from "react";
import { RecipeContext } from "./RecipeProvider";

export default function useRecipe() {
  const context = useContext(RecipeContext);
  if (!context) {
    throw new Error("useRecipe must be used within an RecipeProvider");
  }
  return context;
}