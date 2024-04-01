import styled from '@emotion/styled';
import { useThemeMode } from "../../providers/ThemeMode";

interface IProps {
    links: string[];
}

const NavbarContainer = styled.div`
  position: sticky;
  top: 0;
  padding: 10px 0;
  text-align: center;
  z-index: 1000;
  background-color: ${props => props.theme.navbarBackgroundColor};
`;

const NavbarLink = styled.a`
  text-decoration: none;
  padding: 0 20px;
  color: ${props => props.theme.navbarLinkColor};
`;

const ThemeToggleButton = styled.button`
  cursor: pointer;
  padding: 10px 20px;
  transition: background-color 0.3s ease;
  background-color: ${props => props.theme.buttonBackgroundColor};
  color: ${props => props.theme.buttonTextColor};
  border-radius: 5px;
  &:hover {
    background-color: ${props => props.theme.buttonHoverBackgroundColor};
  }
`;

export default function Navbar({ links }: IProps) {
    const { theme, toggleTheme } = useThemeMode();

    return (
        <NavbarContainer>
            {links.map((link) => (
                <NavbarLink href={`#${link.toLowerCase()}`}>
                    {link}
                </NavbarLink>
            ))}
            <ThemeToggleButton onClick={toggleTheme}>
                {theme === 'dark' ? 'Light Mode' : 'Dark Mode'}
            </ThemeToggleButton>
        </NavbarContainer>
    );
}