import React from "react";
import { IPropsImage } from "./Image";
import Image from "./Image";
import { IPropsIntro } from "./Intro";
import Intro from "./Intro";
import { IPropsContactInfo } from "./ContactInfo";
import ContactInfo from "./ContactInfo";
import { IPropsSection } from "./Section";
import Section from "./Section";
import { IPropsInfoTiles } from "./Elements/InfoTiles";
import InfoTiles from "./Elements/InfoTiles";

interface IPropsCard {
  intro: IPropsIntro;
  image: IPropsImage;
  contactInfo: IPropsContactInfo;
  about: IPropsSection; // TODO create list of sections
  skills: IPropsSection;
}

export default function Card({
  intro,
  image,
  contactInfo,
  about,
  skills,
}: IPropsCard) {
  return (
    <div className="card">
      <Image path={image.path} alt={image.alt} />
      <Intro
        name={intro.name}
        position={intro.position}
        company={intro.company}
      />
      <ContactInfo
        phone={contactInfo.phone}
        email={contactInfo.email}
        web={contactInfo.web}
      />
      <Section title={about.title} content={about.content} />
      <Section
        title={skills.title}
        content={<InfoTiles tiles={skills.content} />}
      />
    </div>
  );
}
