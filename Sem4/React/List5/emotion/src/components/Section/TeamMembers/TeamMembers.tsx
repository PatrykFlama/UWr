import { css } from '@emotion/react';
import styled from '@emotion/styled';
import { lightTheme, darkTheme } from '../../../_variables';
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

const TeamMembersContainer = styled.div`
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
`;

const TeamMember = styled.div`
  flex: 0 0 calc(33.33% - 20px);
  padding: 20px;
  margin: 10px;
  text-align: center;
  img {
    border-radius: 50%;
    margin-bottom: 20px;
  }
  h3 {
    margin-bottom: 10px;
    display: inline-block;
  }
  ${props => props.theme === lightTheme && css`
    background-color: ${lightTheme.teamMemberBackgroundColor};
    color: ${lightTheme.textColor};
  `}
  ${props => props.theme === darkTheme && css`
    background-color: ${darkTheme.teamMemberBackgroundColor};
    color: ${darkTheme.backgroundColor};
  `}
`;

export default function TeamMembers({ teamMembers }: IProps) {
    return (
        <Section id="team">
            <h2>Meet Our Team</h2>
            <TeamMembersContainer>
                {teamMembers.map((member) => (
                    <TeamMember key={member.id}>
                        <img src={member.image} alt={member.name} />
                        <div>
                            <h3>{member.name}</h3>
                            <p>{member.position}</p>
                            <p>{member.bio}</p>
                        </div>
                    </TeamMember>
                ))}
            </TeamMembersContainer>
        </Section>
    );
}