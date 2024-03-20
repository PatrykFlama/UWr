import { useState } from 'react';

interface IProps {
    toggleFavourites: () => void;
    filterOutFavourites: boolean;
    setSearchTerm: (searchTerm: string) => void;
    searchTerm: string;
}

export default function RecipeFilter({ toggleFavourites, filterOutFavourites, setSearchTerm, searchTerm }: IProps) {
    const handleSearchChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const newSearchTerm = e.target.value;
        setSearchTerm(newSearchTerm);
    };

    return (
        <div>
            <input
                type="text"
                placeholder="Search recipes..."
                value={searchTerm}
                onChange={handleSearchChange}
            />
            <button onClick={toggleFavourites}>
                {filterOutFavourites ? 'Show All Recipes' : 'Filter Out Not Favourites'}
            </button>
        </div>
    );
};