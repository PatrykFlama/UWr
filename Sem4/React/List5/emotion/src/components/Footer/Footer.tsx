import { ReactNode } from "react";
import styled from '@emotion/styled';

interface IProps {
    content: ReactNode;
}

const FooterContainer = styled.footer`
  padding: 20px 0;
  text-align: center;
`;

const FooterContent = styled.div``;

export default function Footer({ content }: IProps) {
    return (
        <FooterContainer>
            <FooterContent>
                <p>{content}</p>
            </FooterContent>
        </FooterContainer>
    );
}