import React from "react";
import "./styles.css";

export type sortType = "default" | "az" | "za";
interface IPropsSorter {
  onChange: (sortMode: sortType) => void;
  sortMode: sortType;
}

export default function TaskSorter({ onChange, sortMode }: IPropsSorter) {
  return (
    <div className="taskSorter nice">
      <label htmlFor="sorterSelector">Order by </label>
      <select
        id="sorterSelector"
        value={sortMode}
        onChange={(e) => onChange(e.target.value as sortType)}
      >
        <option value="default">Default</option>
        <option value="az">A-Z</option>
        <option value="za">Z-A</option>
      </select>
    </div>
  );
}
