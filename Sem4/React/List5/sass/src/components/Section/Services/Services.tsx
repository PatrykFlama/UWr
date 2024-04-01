import classes from "./Services.module.scss";
import Section from "../Section";

interface IProps {
    services: {
            id: number;
            name: string;
            description: string;
    }[];
}

export default function Services({ services }: IProps) {
    return (
        <Section id={classes["services"]}>
            <h2>Our Services</h2>
            <ul>
                {services.map((service) => (
                    <li key={service.id}>
                        <h3>{service.name}</h3>
                        <p>{service.description}</p>
                    </li>
                ))}
            </ul>
        </Section>
    );
}


/*
<Section id="services">
<h2>Our Services</h2>
<ul>
  {companyData.services.map((service) => (
    <li key={service.id}>
      <h3>{service.name}</h3>
      <p>{service.description}</p>
    </li>
  ))}
</ul>
</Section>
*/