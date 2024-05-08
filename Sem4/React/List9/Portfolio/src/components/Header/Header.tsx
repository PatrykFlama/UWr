import { Navbar, INavbar } from "./Navbar/Navbar";
import modules from './Header.module.scss';
import MenuIcon from '@mui/icons-material/Menu';
import { useTheme } from "../../providers/Theme";
import { Switch } from "@mui/material";
import Brightness4Icon from '@mui/icons-material/Brightness4';

interface IHeader {
    navigation: INavbar["content"];
}

export function Header({ navigation }: IHeader) {
    const { theme, toggleTheme } = useTheme();


    return (
        <header className={modules.header}>
            <div className={modules.icon}>
                <MenuIcon fontSize="large" />
            </div>
            <div className={modules["filler-long"]} />

            <Navbar content={navigation} />

            <div className={modules["filler-middle"]} />

            <div className={modules["theme-toggle"]}>
                <Brightness4Icon />
                <Switch checked={theme === 'dark'} onChange={toggleTheme} />
            </div>

            <div className={modules["filler-short"]} />
        </header>
    );
}