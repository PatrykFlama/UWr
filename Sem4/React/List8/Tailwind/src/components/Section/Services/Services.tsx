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
        <Section id="services">
            <h2 className="text-2xl font-bold mb-5">Our Services</h2>
            <ul className="list-none p-0 m-0">
                {services.map((service) => (
                    <li key={service.id} className="mb-5">
                        <h3 className="text-xl font-bold mb-2">{service.name}</h3>
                        <p className="text-base">{service.description}</p>
                    </li>
                ))}
            </ul>
        </Section>
    );
}

