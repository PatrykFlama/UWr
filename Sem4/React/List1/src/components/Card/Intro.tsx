import React from "react";

export interface IPropsIntro {
  name: string;
  position: string;
  company: string;
}

export default function Intro({ name, position, company }: IProps) {
  return (
    <div className="intro">
      <h1>{name}</h1>
      <h2> {position}</h2>
      <p>{company}</p>
    </div>
  );
}
