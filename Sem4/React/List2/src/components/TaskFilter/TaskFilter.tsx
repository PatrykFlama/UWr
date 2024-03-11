import "./styles.css";
import React from "react";

interface IProps {
  filterDone: (mode: boolean) => void;
  onFilterName: (data: string) => void;
}

export default function TaskFilter({ filterDone, onFilterName }: IProps) {
  const [taskFilterName, setTaskFilterName] = React.useState("");

  return (
    <div className="taskFilter">
      <form
        className="niceForm"
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

      <div className="switchContainer">
        {/* <span className="switchLabel">Filter Done</span> */}
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
    </div>
  );
}
