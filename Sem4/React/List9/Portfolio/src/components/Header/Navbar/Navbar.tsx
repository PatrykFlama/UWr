import React from 'react';
import modules from './Navbar.module.scss';

export interface INavbar {
    content: [
        {
            title: string,
            ref: string,
            subMenu?: [
                {
                    title: string,
                    ref: string,
                }
            ]
        }
    ]
}

export function Navbar({ content }: INavbar) {
    return (
        <nav className={modules.nav}>
            <ul>
                {content.map((item, index) => (
                    <li key={index} className={modules.li}>
                        {item.ref ? <a href={item.ref} className={modules.a}>{item.title}</a> : item.title}
                        {item.subMenu && (
                            <ul className={modules.ul}>
                                {item.subMenu.map((subItem, subIndex) => (
                                    <li key={subIndex} className={modules.li}>
                                        <a href={subItem.ref} className={modules.a}>{subItem.title}</a>
                                    </li>
                                ))}
                            </ul>
                        )}
                    </li>
                ))}
            </ul>
        </nav>
    );
}
