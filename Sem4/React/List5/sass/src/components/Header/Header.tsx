interface IProps {
    name: string;
    slogan: string;
}

export default function Navbar({ name, slogan }: IProps) {
    return (
        <header id="header" className="header">
            <div className="header-content">
                <h1>{name}</h1>
                <p>{slogan}</p>
            </div>
        </header>
    );
}

/* 
<header id="header" className="header">
<div className="header-content">
    <h1>{companyData.name}</h1>
    <p>{companyData.slogan}</p>
</div>
</header> 
*/