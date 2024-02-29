import React from "react";

export interface IProps {
  title: string;
  content: any;
}

export default function Section({ title, content }: IProps) {
  return (
    <div className="section">
      <h2>{title}</h2>
      <p>{content}</p>
    </div>
  );
}
