import classes from "./TeamMembers.module.scss";
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
            <h2>Meet Our Team</h2>
            <div className={classes["team-members"]}>
                {teamMembers.map((member) => (
                    <div key={member.id} className={classes["team-member"]}>
                        <img src={member.image} alt={member.name} />
                        <div>
                            <h3>{member.name}</h3>
                            <p>{member.position}</p>
                            <p>{member.bio}</p>
                        </div>
                    </div>
                ))}
            </div>
        </Section>
    );
}


/*
<Section id="team">
<h2>Meet Our Team</h2>
<div className="team-members">
  {companyData.teamMembers.map((member) => (
    <div key={member.id} className="team-member">
      <img src={member.image} alt={member.name} />
      <div>
        <h3>{member.name}</h3>
        <p>{member.position}</p>
        <p>{member.bio}</p>
      </div>
    </div>
  ))}
</div>
</Section>
*/