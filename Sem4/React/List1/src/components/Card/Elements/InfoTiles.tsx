import React from "react";
import "./styles.css";

export interface IPropsInfoTiles {
  tiles: {
    icon: string;
    label: string;
  }[];
}

export default function InfoTiles({ tiles }: IPropsInfoTiles) {
  return (
    <div className="info-tiles">
      {tiles.map((tile) => {
        return (
          <div className="info-tile">
            <img className="icon" src={tile.icon} />
            <div>{tile.label}</div>
          </div>
        );
      })}
    </div>
  );
}
