import "./styles.css";
import React from "react";
import Task from "./components/Task/Task";
import TaskAdder from "./components/TaskAdder/TaskAdder";
import TaskFilter from "./components/TaskFilter/TaskFilter";
import DoneFilter from "./components/TaskFilter/DoneFilter";
import TaskSorter from "./components/TaskFilter/TaskSorter";
import { sortType } from "./components/TaskFilter/TaskSorter";
import Pages from "./components/TaskFilter/Pages";

const initialList = [
  { title: "element1", done: false },
  { title: "element2", done: true },
  { title: "element3", done: false },
];

export default function App() {
  const [list, setList] = React.useState(initialList);
  const [filterDone, setFilterDone] = React.useState(false);
  const [searchString, setSearchString] = React.useState("");
  const [sortMode, setSortMode] = React.useState<sortType>("default");
  const [IPP, setIPP] = React.useState(4); // items per page
  const [page, setPage] = React.useState(0);

  const calcMaxPages = () =>
    Math.floor(
      (list.filter(
        (task) =>
          (task.title.includes(searchString) && filterDone && !task.done) ||
          (!filterDone && task.done)
      ).length -
        1) /
        IPP
    );

  return (
    <div className="App">
      <h1>{filterDone ? "TODO LIST" : "DONE LIST"}</h1>
      <TaskAdder
        onAdd={(title) => setList([...list, { title, done: false }])}
      />
      <TaskFilter onFilterName={(str) => setSearchString(str)} />

      <div className="fitInRow">
        <DoneFilter filterDone={(filter) => setFilterDone(filter)} />
        <TaskSorter
          sortMode={sortMode}
          onChange={(newSortMode) => {
            if (newSortMode != sortMode) {
              setSortMode(newSortMode);
            }
          }}
        />
      </div>

      <Pages
        updateIPP={(newNumber) => setIPP(newNumber)}
        IPP={IPP}
        updatePage={(newPage) => {
          const maxPages = calcMaxPages();

          if (newPage < 0) newPage = 0;
          else if (newPage > maxPages) newPage = maxPages;
          setPage(newPage);
        }}
        page={page}
      />

      {list
        .filter(
          (task) =>
            (task.title.includes(searchString) && filterDone && !task.done) ||
            (!filterDone && task.done)
        )
        .sort((a, b) => {
          if (sortMode == "az") return a.title > b.title ? 1 : -1;
          else if (sortMode == "za") return a.title < b.title ? 1 : -1;
          return 0;
        })
        .slice(page * IPP, (page + 1) * IPP)
        .map((task) => (
          <Task
            title={task.title}
            done={task.done}
            onDone={() => {
              const newList = [...list];
              newList.map((obj) => {
                if (obj === task) {
                  obj.done = !obj.done;
                }
              });
              setList(newList);
            }}
            onDelete={() => {
              const newList = list.filter(function (obj) {
                return obj !== task;
              });
              setList(newList);
            }}
          />
        ))}
    </div>
  );
}
