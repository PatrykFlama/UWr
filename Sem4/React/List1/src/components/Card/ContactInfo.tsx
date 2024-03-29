import React from "react";
import { IPropsInfoCard } from "./Elements/InfoCard";
import InfoCard from "./Elements/InfoCard";

export interface IPropsContactInfo {
  phone: IPropsInfoCard;
  email: IPropsInfoCard;
  web: IPropsInfoCard;
}

export default function ContactLabel({ phone, email, web }: IPropsContactInfo) {
  return (
    <div className="contact-info">
      <InfoCard
        icon={phone.icon}
        label={phone.label}
        href={`tel:${phone.label}`}
      />
      <InfoCard
        icon={email.icon}
        label={email.label}
        href={`mailto:${email.label}`}
      />
      <InfoCard icon={web.icon} label={web.label} href={web.label} />
    </div>
  );
}
