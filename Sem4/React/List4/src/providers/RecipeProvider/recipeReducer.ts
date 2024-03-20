import { IRecipe } from "../../types/Recipe.type";

type RecipeAction =
  | {
      type: "ADD";
      payload: {
        title: string;
        ingredients: string[];
        steps: string[];
      };
    }
  | {
      type: "REMOVE";
      payload: {
        id: number;
      };
    }
  | {
      type: "TOGGLE_FAVOURITE",
      payload: {
        id: number;
      }
  };

export const recipeReducer = (state: IRecipe[], action: RecipeAction) => {
  console.log(action);
  switch (action.type) {
    case "ADD":
      return [
        ...state,
        {
          id: Math.random(),
          title: action.payload.title,
          ingredients: action.payload.ingredients,
          steps: action.payload.steps,
          is_favourite: false,
        },
      ];
    case "REMOVE":
      return state.filter((recipe) => recipe.id !== action.payload.id);
    case "TOGGLE_FAVOURITE":
      return state.map((recipe) => (
        recipe.id !== action.payload.id ? recipe : {...recipe, is_favourite: !recipe.is_favourite}))
    default:
      return state;
  }
};