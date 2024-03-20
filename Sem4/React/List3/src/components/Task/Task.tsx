import "./styles.css";
import React, { useState } from "react";

interface IProps {
  title: string;
  done?: boolean;
  onDone: () => void;
  onDelete: () => void;
}

export default function Task({
  title,
  done = false,
  onDone,
  onDelete,
}: IProps) {
  const [insideDone, setInsideDone] = useState(done);

  const handleDeleteClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    event.stopPropagation(); // Prevent event propagation to the parent div
    onDelete(); // Call onDelete handler
  };

  return (
    <div className={`task ${done ? "done" : ""}`}>
      <button
        className={`interactiveButton ${done ? "doneButton" : "unodneButton"}`}
        onClick={onDone}
      >
        {done ? "UNDO" : "DONE"}
      </button>
      <h3>{title}</h3>

      <button className="deleteButton" onClick={handleDeleteClick}>
        DELETE
      </button>
    </div>
  );
}
