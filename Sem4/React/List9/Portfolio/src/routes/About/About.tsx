import modules from './About.module.scss';
import { Typography } from '@mui/material';
import totallyme from '../../assets/hehe.jpg';

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
                <img src={totallyme} alt="To sem jo" />
            </div>
        </div>
    );
};

export default About;