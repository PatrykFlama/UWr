import React from "react";

export interface IPropsImage {
  path: string;
  alt: string;
}

export default function Image({ path, alt }: IPropsImage) {
  return (
    <div className="image">
      <img src={path} alt={alt} />
    </div>
  );
}
