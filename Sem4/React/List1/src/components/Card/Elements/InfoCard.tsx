import React from "react";
import "./styles.css";

export interface IPropsInfoCard {
  icon: string;
  label: string;
  href?: string;
}

export default function InfoCard({ icon, label, href = "" }: IPropsInfoCard) {
  return (
    <div className="contact-label">
      <img className="icon" src={icon} />
      <a href={href}>
        <div>{label}</div>
      </a>
    </div>
  );
}
