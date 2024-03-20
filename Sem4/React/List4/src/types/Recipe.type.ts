export interface IRecipe {
    id: number;
    title: string;
    ingredients: string[];
    steps: string[];
    is_favourite: boolean;
}