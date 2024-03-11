import "./styles.css";
import React from "react";

interface IProps {
  onAdd: (data: string) => void;
}

export default function TaskAdder({ onAdd }: IProps) {
  const [taskName, setTaskName] = React.useState("");
  return (
    <div className="taskAdder">
      <form
        className="niceForm"
        onSubmit={(e) => {
          e.preventDefault();
          if (taskName != "") onAdd(taskName);
          setTaskName("");
        }}
      >
        <input
          placeholder="Name of task to add"
          value={taskName}
          onChange={(e) => setTaskName(e.target.value)}
        />
        <button type="submit">Add task</button>
      </form>
    </div>
  );
}
