import Section from "../Section";

interface IProps {
    handleSubmit: (event: React.FormEvent<HTMLFormElement>) => void;
}

export default function Contact({ handleSubmit }: IProps) {
    return (
        <Section id="contact">
            <div className="mb-8">
                <h2 className="text-2xl font-bold mb-5">Contact Us</h2>
                <form
                    onSubmit={handleSubmit}
                    className="max-w-md mx-auto p-4 bg-white dark:bg-slate-500 rounded-lg shadow-lg"
                >
                    <div className="mb-4">
                        <input
                            type="text"
                            placeholder="Name"
                            className="w-full px-4 py-2 rounded border-gray-300 dark:border-slate-500 focus:outline-none focus:border-green-400"
                            required
                        />
                    </div>
                    <div className="mb-4">
                        <input
                            type="email"
                            placeholder="Email"
                            className="w-full px-4 py-2 rounded border-gray-300 dark:border-slate-500 focus:outline-none focus:border-green-400"
                            required
                        />
                    </div>
                    <div className="mb-4">
                        <textarea
                            rows={5}
                            placeholder="Message"
                            className="w-full px-4 py-2 rounded border-gray-300 dark:border-slate-800 resize-none focus:outline-none focus:border-green-400"
                            required
                        ></textarea>
                    </div>
                    <button
                        type="submit"
                        className="w-full px-4 py-2 rounded bg-green-500 text-white hover:bg-green-600 focus:outline-none focus:bg-green-600 transition duration-300"
                    >
                        Send Message
                    </button>
                </form>
            </div>
        </Section>
    );
}
