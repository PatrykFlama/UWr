import "./styles.css";
import Card from "./components/Card/Card";

export default function App() {
  return (
    <main>
      <Card
        image={{
          path: "https://via.placeholder.com/350",
          alt: "image alt",
        }}
        intro={{
          name: "John Doe",
          position: "Frontend Developer",
          company: "Company",
        }}
        contactInfo={{
          phone: {
            icon: "./icons/phone.png",
            label: "123 123 123",
          },
          email: {
            icon: "./icons/email.png",
            label: "m@i.l",
          },
          web: {
            icon: "./icons/web.png",
            label: "me.com",
          },
        }}
        about={{
          title: "About me",
          content:
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad ",
        }}
        skills={{
          title: "Skills",
          content: [
            {
              icon: "https://cdn.iconscout.com/icon/free/png-512/free-react-1-282599.png",
              label: "React",
            },
            {
              icon: "https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg",
              label: "Python",
            },
            {
              icon: "https://cdn.iconscout.com/icon/free/png-512/free-react-1-282599.png",
              label: "React",
            },
            {
              icon: "https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg",
              label: "Python",
            },
            {
              icon: "https://cdn.iconscout.com/icon/free/png-512/free-react-1-282599.png",
              label: "React",
            },
            {
              icon: "https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg",
              label: "Python",
            },
            {
              icon: "https://cdn.iconscout.com/icon/free/png-512/free-react-1-282599.png",
              label: "React",
            },
          ],
        }}
      />
    </main>
  );
}
