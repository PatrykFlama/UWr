import modules from "./Contact.module.scss";
import { Container, Typography, TextField, Button, Box } from "@mui/material";
import EmailIcon from '@mui/icons-material/Email';
import GitHubIcon from '@mui/icons-material/GitHub';
import LinkedInIcon from '@mui/icons-material/LinkedIn';
import Link from '@mui/material/Link';


function Contact() {

    return (
        <div className={modules.main}>
            <Typography variant="h4" component="h1" align="center">
                Contact me (why not?)
            </Typography>
            <div className={modules.box}>
                <EmailIcon /> <Link href="mailto:patrykflama@gmail.com" underline="none" sx={{ fontSize: '1.2em', color: 'inherit', marginLeft: '0.5em' }}>patrykflama@gmail.com</Link>
            </div>
            <div className={modules.box}>
                <GitHubIcon /> <Link href="https://github.com/PatrykFlama" target="_blank" underline="none" sx={{ fontSize: '1.2em', color: 'inherit', marginLeft: '0.5em' }}>PatrykFlama</Link>
            </div>
            <div className={modules.box}>
                <LinkedInIcon /> <Link href="https://www.linkedin.com/in/patryk-flama/" target="_blank" underline="none" sx={{ fontSize: '1.2em', color: 'inherit', marginLeft: '0.5em' }}>patryk-flama</Link>
            </div>


        </div>
    );
}

export default Contact;
