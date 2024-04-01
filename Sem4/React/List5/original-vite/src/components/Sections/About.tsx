import Section from "../Section";

export default function About({ companyData }: { companyData: string }) {
    return (
        <Section id="about">
            <h2>About Us</h2>
            <p>{companyData}</p>
        </Section>
    );
}


/*
<h2>About Us</h2>
<p>{companyData.about}</p>
*/