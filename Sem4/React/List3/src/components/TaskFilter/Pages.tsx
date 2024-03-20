// IPP = Items Per Page
import React from "react";
import "./styles.css";

export type sortType = "default" | "az" | "za";
interface IPropsSorter {
  updateIPP: (newIPP: number) => void;
  IPP: number;
  updatePage: (newPage: number) => void;
  page: number;
}

export default function Pages({
  updateIPP,
  IPP,
  updatePage,
  page,
}: IPropsSorter) {
  return (
    <div className="pages fitInRow">
      <div className="pageSetting">
        <label htmlFor="ipp">Items per page</label>
        <input
          className="ipp"
          id="ipp"
          type="number"
          value={IPP}
          onChange={(e) => updateIPP(Number(e.target.value))}
          placeholder="Items per page"
        />
      </div>

      <div className="pageSetting">
        <label htmlFor="page">Page</label>
        <input
          className="page"
          id="page"
          type="number"
          value={page}
          onChange={(e) => updatePage(Number(e.target.value))}
          placeholder="Page"
        />
      </div>
    </div>
  );
}
