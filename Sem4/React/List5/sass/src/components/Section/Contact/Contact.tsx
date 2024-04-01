import classes from "./Contact.module.scss";
import Section from "../Section";


interface IProps {
    handleSubmit: (event: React.FormEvent<HTMLFormElement>) => void;
}

export default function Contact({ handleSubmit }: IProps) {
    return (
        <Section id={classes["contact"]}>
            <h2>Contact Us</h2>
            <form onSubmit={handleSubmit} className={classes["contact-form"]}>
                <div className={classes["form-group"]}>
                    <input type="text" placeholder="Name" required />
                </div>
                <div className={classes["form-group"]}>
                    <input type="email" placeholder="Email" required />
                </div>
                <div className={classes["form-group"]}>
                    <textarea rows={5} placeholder="Message" required></textarea>
                </div>
                <button type="submit">Send Message</button>
            </form>
        </Section>
    );
}


/*
<Section id="contact">
    <h2>Contact Us</h2>
    <form onSubmit={handleSubmit} className="contact-form">
    <div className="form-group">
        <input type="text" placeholder="Name" required />
    </div>
    <div className="form-group">
        <input type="email" placeholder="Email" required />
    </div>
    <div className="form-group">
        <textarea rows={5} placeholder="Message" required></textarea>
    </div>
    <button type="submit">Send Message</button>
    </form>
</Section>
*/