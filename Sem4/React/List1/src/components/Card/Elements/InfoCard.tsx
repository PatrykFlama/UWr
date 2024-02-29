import React from "react";
import "./styles.css";

export interface IPropsInfoCard {
  icon: string;
  label: string;
}

export default function InfoCard({ icon, label }: IPropsInfoCard) {
  return (
    <div className="contact-label">
      <img className="icon" src={icon} />
      <div>{label}</div>
    </div>
  );
}
