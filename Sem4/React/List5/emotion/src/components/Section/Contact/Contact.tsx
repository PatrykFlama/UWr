import React from "react";
import styled from '@emotion/styled';
import Section from "../Section";

interface IProps {
    handleSubmit: (event: React.FormEvent<HTMLFormElement>) => void;
}

const ContactContainer = styled.div`
  margin-bottom: 40px;
`;

const ContactForm = styled.form`
  max-width: 500px;
  margin: 0 auto;
  padding: 20px;
  border-radius: 10px;
  display: flex;
  flex-direction: column;
  background-color: ${props => props.theme.contactFormBackgroundColor};
  color: ${props => props.theme.textColor};
  border: 1px solid ${props => props.theme.contactFormBorderColor};
`;

const FormGroup = styled.div`
  margin-bottom: 20px;
`;

const Input = styled.input`
  width: calc(100% - 20px);
  padding: 10px;
  border-radius: 5px;
  border: none;
  margin-top: 5px;
  background-color: ${props => props.theme.inputBackgroundColor};
  color: ${props => props.theme.textColor};
  border: 1px solid ${props => props.theme.inputBorderColor};
`;

const TextArea = styled.textarea`
  resize: vertical;
  width: calc(100% - 20px);
  padding: 10px;
  border-radius: 5px;
  border: none;
  margin-top: 5px;
  background-color: ${props => props.theme.inputBackgroundColor};
  color: ${props => props.theme.textColor};
  border: 1px solid ${props => props.theme.inputBorderColor};
`;

const Button = styled.button`
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
  cursor: pointer;
  transition: background-color 0.3s ease;
  background-color: ${props => props.theme.buttonBackgroundColor};
  color: ${props => props.theme.buttonTextColor};
  &:hover {
    background-color: ${props => props.theme.buttonHoverBackgroundColor};
  }
`;

export default function Contact({ handleSubmit }: IProps) {
    return (
        <Section id="contact">
            <ContactContainer>
                <h2>Contact Us</h2>
                <ContactForm onSubmit={handleSubmit}>
                    <FormGroup>
                        <Input type="text" placeholder="Name" required />
                    </FormGroup>
                    <FormGroup>
                        <Input type="email" placeholder="Email" required />
                    </FormGroup>
                    <FormGroup>
                        <TextArea rows={5} placeholder="Message" required />
                    </FormGroup>
                    <Button type="submit">Send Message</Button>
                </ContactForm>
            </ContactContainer>
        </Section>
    );
}