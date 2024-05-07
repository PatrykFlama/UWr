import modules from "./Navbar.module.scss";
import { NavLink } from "react-router-dom";

export interface INavbar {
    content: [
        {
            label: string;
            ref: string;
        }
    ];
}

export function Navbar({ content }: INavbar) {
    return (
        <div className={modules.nav}>
            {content.map((item, index) => (
                <NavLink to={item.ref} className={modules.a}>
                    <div className={modules.li}>
                        {item.label}
                    </div>
                </NavLink>
            ))}
        </div>
    );
}
