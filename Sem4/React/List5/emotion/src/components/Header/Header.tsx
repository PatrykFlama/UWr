import styled from '@emotion/styled';

interface IProps {
    name: string;
    slogan: string;
}

const HeaderContainer = styled.header`
  padding: 50px 0;
  text-align: center;
`;

const HeaderTitle = styled.h1`
  font-size: 3em;
  margin-bottom: 10px;
`;

const HeaderSlogan = styled.p`
  font-size: 1.5em;
`;

export default function Navbar({ name, slogan }: IProps) {
    return (
        <HeaderContainer id="header">
            <div>
                <HeaderTitle>{name}</HeaderTitle>
                <HeaderSlogan>{slogan}</HeaderSlogan>
            </div>
        </HeaderContainer>
    );
}