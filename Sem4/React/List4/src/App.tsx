import React from 'react';
import { useState } from 'react';
import './App.css';
import RecipeProvider from './providers/RecipeProvider/RecipeProvider';
import RecipeAdder from './components/RecipeAdder';
import RecipeFilter from './components/RecipeFilter';
import RecipeList from './components/RecipeList/RecipeList';

export default function App() {
  const [ favouritesFilter, setFavouritesFilter ] = useState(false);
  const [searchTerm, setSearchTerm] = useState('');

  return (
    <div className='App'>
      <RecipeProvider>
        <RecipeAdder/>
        <RecipeFilter 
          toggleFavourites={() => setFavouritesFilter(!favouritesFilter)}
          filterOutFavourites={favouritesFilter}
          setSearchTerm={setSearchTerm}
          searchTerm={searchTerm} />
        <RecipeList onlyFavourites={favouritesFilter} filterString={searchTerm}/>
      </RecipeProvider>
    </div>
  )
}
