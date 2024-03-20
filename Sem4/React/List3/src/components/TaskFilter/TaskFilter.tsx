import "./styles.css";
import React from "react";

interface IProps {
  onFilterName: (data: string) => void;
}

export default function TaskFilter({ onFilterName }: IProps) {
  const [taskFilterName, setTaskFilterName] = React.useState("");

  return (
    <div className="taskFilter">
      <form
        className="nice"
        onSubmit={(e) => {
          e.preventDefault();
          onFilterName(taskFilterName);
        }}
      >
        <input
          placeholder="Name of tasks to filter"
          value={taskFilterName}
          onChange={(e) => setTaskFilterName(e.target.value)}
        />
        <button type="submit">Filtler tasks</button>
      </form>
    </div>
  );
}
