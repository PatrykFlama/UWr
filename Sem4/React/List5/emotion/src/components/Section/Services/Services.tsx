import styled from '@emotion/styled';
import Section from "../Section";

interface IProps {
    services: {
        id: number;
        name: string;
        description: string;
    }[];
}

const ServicesList = styled.ul`
  list-style: none;
  padding: 0;
  margin: 0;
`;

const ServiceItem = styled.li`
  margin-bottom: 20px;
  text-align: left;
`;

const ServiceTitle = styled.h3`
  font-size: 1.8em;
  margin-bottom: 10px;
`;

export default function Services({ services }: IProps) {
    return (
        <Section id="services">
            <h2>Our Services</h2>
            <ServicesList>
                {services.map((service) => (
                    <ServiceItem key={service.id}>
                        <ServiceTitle>{service.name}</ServiceTitle>
                        <p>{service.description}</p>
                    </ServiceItem>
                ))}
            </ServicesList>
        </Section>
    );
}