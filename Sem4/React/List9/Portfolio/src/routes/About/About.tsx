import modules from './About.module.scss';
import { Typography, Container } from '@mui/material';

function About() {
    return (
        <div className={modules.content}>
            <div className={modules.left}>
                <Typography variant="h5" component="h1" align="center">
                    Hi, I'm
                </Typography>
                <Typography variant="h2" component="h1" align="center">
                    Patryk Flama
                </Typography>
                <Typography variant="body1" align="center">
                    IT and math freak from University of Wrocław
                </Typography>
            </div>
            <div className={modules.right}>
                <img src="https://i.pinimg.com/736x/31/e2/a1/31e2a17fc75dd7d332086446d8ebb40d.jpg" alt="To sem jo" />
            </div>
        </div>
    );
};

export default About;