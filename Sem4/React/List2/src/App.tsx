import "./styles.css";
import React from "react";
import Task from "./components/Task/Task";
import TaskAdder from "./components/TaskAdder/TaskAdder";
import TaskFilter from "./components/TaskFilter/TaskFilter";

const initialList = [
  { title: "element1", done: false },
  { title: "element2", done: true },
  { title: "element3", done: false },
];

export default function App() {
  const [list, setList] = React.useState(initialList);
  const [filterDone, setFilterDone] = React.useState(false);
  const [searchString, setSearchString] = React.useState("");

  return (
    <div className="App">
      <h1>{filterDone ? "TODO LIST" : "DONE LIST"}</h1>
      <TaskAdder
        onAdd={(title) => setList([...list, { title, done: false }])}
      />
      <TaskFilter
        filterDone={(filter) => setFilterDone(filter)}
        onFilterName={(str) => setSearchString(str)}
      />

      {list.map((task, index) => {
        if (
          task.title.includes(searchString) &&
          ((filterDone && !task.done) || (!filterDone && task.done))
        )
          return (
            <Task
              title={task.title}
              done={task.done}
              onClick={() => {
                //! to nie do końca jest ok
                const newList = [...list];

                newList[index].done = !newList[index].done;
                setList(newList);
              }}
              onDelete={() => {
                const newList = [...list];
                newList.splice(index, 1);
                setList(newList);
              }}
            />
          );
      })}
    </div>
  );
}
