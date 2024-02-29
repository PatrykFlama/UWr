import React from "react";

export interface IPropsSection {
  title: string;
  content: any;
}

export default function Section({ title, content }: IPropsSection) {
  return (
    <div className="section">
      <h2>{title}</h2>
      <p>{content}</p>
    </div>
  );
}
