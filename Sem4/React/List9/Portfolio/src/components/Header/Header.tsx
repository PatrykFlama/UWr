import { Navbar, INavbar } from "./Navbar/Navbar";
import modules from './Header.module.scss';

interface IHeader {
    title: string;
    content: INavbar['content'];
}

export function Header({ title, content }: IHeader) {
    return (
        <header className={modules.header}>
            <h1>{title}</h1>
            <Navbar content={content} />
        </header>
    );
}