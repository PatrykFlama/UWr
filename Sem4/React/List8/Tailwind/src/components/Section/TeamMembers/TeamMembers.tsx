import Section from "../Section";

interface IProps {
    teamMembers: {
        id: number;
        name: string;
        position: string;
        bio: string;
        image: string;
    }[];
}

export default function TeamMembers({ teamMembers }: IProps) {
    return (
        <Section id="team">
            <h2 className="text-2xl font-bold mb-5">Meet Our Team</h2>
            <div className="flex flex-wrap justify-center">
                {teamMembers.map((member) => (
                    <div key={member.id} className="flex flex-col items-center w-1/3 sm:w-1/4 md:w-1/5 p-4 m-2 text-center bg-gray-200 dark:bg-gray-500 rounded-lg">
                        <img src={member.image} alt={member.name} className="rounded-full w-24 h-24 mb-4" />
                        <div>
                            <h3 className="text-lg font-bold mb-2">{member.name}</h3>
                            <p className="text-sm font-semibold mb-2">{member.position}</p>
                            <p className="text-sm">{member.bio}</p>
                        </div>
                    </div>
                ))}
            </div>
        </Section>
    );
}
