import React from "react";
import styled from '@emotion/styled';

const SectionContainer = styled.section`
  padding: 20px 0;
  &:nth-child(even) {
    background-color: ${props => props.theme.teamMemberBackgroundColor};
  }
  max-width: 800px;
  margin: 0 auto;
`;

interface IProps {
    id: string;
    children: React.ReactNode;
}

export default function Section({ id, children }: IProps) {
    return (
        <SectionContainer id={id}>
            {children}
        </SectionContainer>
    );
}