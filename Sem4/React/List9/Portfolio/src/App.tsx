import "./App.scss";
import { ThemeProvider } from "./providers/Theme";
import ThemeWrapper from "./components/ThemeWrapper/ThemeWrapper";

import {
    RouterProvider,
    createBrowserRouter,
} from "react-router-dom";

import RootLayout from "./layouts/RootLayout/RootLayout";
import About from "./routes/About/About";
import Projects from "./routes/Projects/Projects";
import Contact from "./routes/Contact/Contact";
import Experience from "./routes/Experience/Experience";

const router = createBrowserRouter([
    {
        path: "/",
        element: <RootLayout />,
        children: [
            {
                path: "/",
                element: <About />,
            },
            {
                path: "/about",
                element: <About />,
            },
            {
                path: "/projects",
                element: <Projects />,
            },
            {
                path: "/experience",
                element: <Experience />,
            },
            {
                path: "/contact",
                element: <Contact />,
            },
        ],
    },
]);

function App() {
    return (
        <ThemeProvider>
            <ThemeWrapper>
                <RouterProvider router={router} />
            </ThemeWrapper>
        </ThemeProvider>
    );
}

export default App;
