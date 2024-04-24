import { ReactNode } from 'react';

export default function Footer({ content }: { content: ReactNode }) {
    return (
        <footer className="p-5 text-center">
            <div>
                <p>{content}</p>
            </div>
        </footer>
    );
}

