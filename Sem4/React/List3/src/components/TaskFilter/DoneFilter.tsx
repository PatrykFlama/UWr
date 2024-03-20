import "./styles.css";
import React from "react";

interface IProps {
  filterDone: (mode: boolean) => void;
}

export default function DoneFilter({ filterDone }: IProps) {
  return (
    <div className="switchContainer">
      <label className="switch">
        <input
          type="checkbox"
          onChange={(e) => {
            filterDone(e.target.checked);
          }}
        />
        <span className="slider round"></span>
      </label>
    </div>
  );
}
